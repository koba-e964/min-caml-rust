#[derive(Debug)]
pub struct RegisterNameError(pub String);

#[derive(Debug)]
pub enum Error {
    RegisterNameError(RegisterNameError),
}

impl From<RegisterNameError> for Error {
    fn from(err: RegisterNameError) -> Self {
        Error::RegisterNameError(err)
    }
}
