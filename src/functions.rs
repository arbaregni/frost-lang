use crate::type_inference::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub in_types: Vec<Type>,
    pub out_type: Box<Type>,
}
impl std::convert::Into<Type> for FnType {
    fn into(self) -> Type {
        Type::Fn(self)
    }
}