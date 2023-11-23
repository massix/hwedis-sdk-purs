import toml from 'toml';

export const parseImpl = configFile => Left => Right => () => {
  try {
    return Right(toml.parse(configFile));
  } catch (e) {
    return Left(e.message);
  }
};

