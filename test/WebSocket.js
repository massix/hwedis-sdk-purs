import { GenericContainer } from "testcontainers";

export const mkGenericContainerImpl = StoppedContainer => image => () => {
  const container = new GenericContainer(image);
  return StoppedContainer(container);
};

export const exposePortImpl = StoppedStartedContainer => container => port => () => {
  return StoppedStartedContainer(container.withExposedPorts(port));
};

export const containerHostImpl = container => () => {
  return container.getHost();
};

export const containerPortImpl = container => mappedPort => () => {
  return container.getMappedPort(mappedPort);
};

export const startImpl = StartedContainer => container => async () => {
  return StartedContainer(await container.start());
};

export const stopImpl = StoppedContainer => container => async () => {
  await container.stop();
  return StoppedContainer(container);
};

