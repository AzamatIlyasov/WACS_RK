
from config.logger import logger

import pandas as pd   # must be replaced with internal python tools!
import datetime
from docker import DockerClient
from docker.errors import DockerException, APIError, ContainerError, ImageNotFound

import os
import time

class Operator():

    def __init__(self):

        try:
            self.client = DockerClient(base_url='unix://var/run/docker.sock',timeout=10)
            logger.debug(f'Docker client created {self.client} \n {self.client.events()}')
            logger.error(f'Client API access is: {self.client.ping()}')
        except DockerException as exc:
            logger.error(f'Connection with docker.socket aborted {exc}')
            raise exc

        self.history = None
        self.future = None

        self.start = None
        self.stop = None

        self.host_name = None
        self.container_id = None
        self.service_response = None
        self.container_state = None

    def _runContainer(self, image, volumes, ports):

        '''Run a docker container using a given image; passing keyword arguments
        documented to be accepted by docker's client.containers.run function
        No extra side effects. Handles and reraises ContainerError, ImageNotFound,
        and APIError exceptions.
        '''

        #network = os.getenv('SERVICES_NETWORK', default='service_network')
        #con_mem_limit = os.getenv('CONTAINER_MEM_LIMIT', default=536870912) #512Mb -> bytes
        con_mem_limit="512m"
        #ports = {ports:None}
        container = None

        try:
            network = 'service_network'
            #self.container_name = 'point'
            self.container_name = f'point-{self.point}'
            container = self.client.containers.run(
                image,
                name=self.container_name,
                #ports=ports,
                volumes=volumes,
                detach=True,
                mem_limit=con_mem_limit,
                cpuset_cpus="1",
                network=network
                )
            if "Name" in container.attrs.keys():
                logger.debug(f'Docker container created \n {self.client.events(decode=True)}')
        except ContainerError as exc:
            logger.error(f"ContainerError: {exc}")
            raise exc
        except ImageNotFound as exc:
            logger.error(f"ImageNotFound: {exc}")
            raise exc
        except APIError as exc:
            logger.error(f"APIError: {exc}")
            raise exc

        return container

    def _removeContainer(self):

            '''
            Remove a docker container using a given id; passing keyword arguments
            documented to be accepted by docker's client.containers.remove function
            No extra side effects. Handles and reraises APIError exceptions.
            '''

            try:

                container = self.client.containers.get(container_id=self.container_id)
                container.remove(force=True)

                logger.info(f'Container {self.container_id} was removed')
                self.container_id = None

            except APIError as exc:
                logger.error(f'Unhandled APIError error: {exc}')
                raise exc

    def run_steps(self, data, config, service):

        image = config.get('image')
        volumes = config.get('volumes')
        app_port = config.get('app_port')
        host_name = config.get('host_name')

        self.point = data.get('model_settings').get('point')

        self.start = str(datetime.datetime.now())
        self.container_id = None

        count = 0
        containet_state = ''
        con_ext_port = None

        try:

            while True:

                # step 1. Create and run container
                if self.container_id == None:

                    self.container = self._runContainer(
                        image,
                        volumes,
                        ports=app_port
                        )
                    self.container_id = self.container.short_id
                    logger.debug(self.container_id)
                #
                elif containet_state.lower() == 'exited':
                    logger.warning(f'Container {self.container_id} unexpected exited')
                    break

                # step 3. Try to get information about container
                elif containet_state.lower() == 'created':
                    time.sleep(5)
                    count = count + 1

                    self.container = self.client.containers.get(container_id=self.container_id)

                # step 2. Send data and get prediction
                elif containet_state.lower() == 'running':

                    payload = service.handleRequest(data)

                    self.service_response = service.call(self.container_name, app_port=8005, payload=payload)

                    logger.info(self.service_response)

                    break

                elif count >= 5:
                    logger.warning('Max retries exeeded')
                    break

                containet_state = self.container.attrs['State'].get('Status')

            self.stop = str(datetime.datetime.now())

            response = {
                        "metadata": {
                            "containerId": self.container_id,
                            "point": self.point,
                            "start_time": self.start,
                            "finish_time": self.stop
                            },
                        "state":self.service_response['state'],
                        "response":self.service_response['result'],
                        "predictions":self.service_response['prediction']
            }

            return response

        except (APIError, DockerException) as exc:

            logger.error(f'Error create docker: {exc}')
            raise exc

        finally:

            # step 4.1 stop and remove a container
            self._removeContainer()


class Test(object):

    def __init__(self):

        self.start = None
        self.finish = None

        self.config = None
        self.type = None
        self.point = None

        self.response = None
        self.dataset = {}

        self.time_steps = None
        self.time_freq = None

        self.regressor = None
        self.settings = None
    
    #Only for test.
    def job(self, data):

        self.config = data['model_settings']
        self.dataset = data['data']
        # self.regressor = data['regressor']
        self.time_steps = self.config['time_steps']
        self.time_freq = self.config['time_freq']
        self.rolling_window = self.config['rolling']

        hist_test_df = pd.DataFrame(self.dataset, columns=['ds','y','x'])
        hist_test_df['ds'] = pd.to_datetime(hist_test_df['ds'], unit='ms')

        hist_test_df = hist_test_df.set_index(['ds'])
        hist_test_df = hist_test_df.astype('float')

        last_date = hist_test_df.index[-1:]
        time_freq = self.time_freq

        [date_year] = last_date.year.tolist()
        [date_month] = last_date.month.tolist()
        [date_day] = last_date.day.tolist()
        [date_hour] = last_date.hour.tolist()

        date_time = datetime.datetime(date_year, \
                month=date_month, \
                day=date_day, \
                hour=date_hour, \
                minute=0)

        dtr = pd.date_range(start=date_time, periods=len(hist_test_df), freq=time_freq)
        predict_test_df = pd.DataFrame({'date_time':dtr})

        transform = pd.DataFrame(data=hist_test_df['y'].values, columns=['y'])
        transform = transform.iloc[::-1]
        # logger.info(transform)
        transform = transform.rolling(window=self.rolling_window).sum()/self.rolling_window
        # logger.info(transform)
        transform = transform.iloc[::-1]
        # logger.info(transform)
        transform.fillna(method='ffill', inplace=True)
        # logger.info(transform)
        answer = transform['y'].values.tolist()
        self.response = {"predictions": answer[:47]}

        return self.response
