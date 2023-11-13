"use strict";

import { GenericContainer, Wait } from "testcontainers";


export const mkGenericContainerImpl = StoppedContainer => image => () => {
  const container = new GenericContainer(image)/* .withEnvironment({}) */;
  return StoppedContainer(container);
};

export const setEnvironmentImpl = container => env => () => {
  const toEnv = env.reduce((acc, { key, value }) => { 
    acc[key] = value;
    return acc;
  }, {});
  container.withEnvironment(toEnv);
};

export const waitStrategyImpl = container => waitStrategy => () => {
  if (waitStrategy.constructor.name === 'LogWaitStrategy') {
    if (waitStrategy.value1.constructor.name == 'Just') {
      container.withWaitStrategy(Wait.forLogMessage(waitStrategy.value0, waitStrategy.value1.value0));
    } else {
      container.withWaitStrategy(Wait.forLogMessage(waitStrategy.value0));
    }

  } else if (typeof waitStrategy === 'StartupTimeout') {
    container.withStartupTimeout(waitStrategy.value0);
  }
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

