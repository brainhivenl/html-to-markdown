use html5ever::Attribute;

pub trait AttributeList {
    fn get(&self, name: &str) -> Option<&str>;
    fn get_or_default(&self, name: &str) -> String;
}

impl AttributeList for &[Attribute] {
    fn get(&self, name: &str) -> Option<&str> {
        self.iter().find_map(|a| {
            if a.name.prefix.is_none() && &a.name.local == name {
                Some(a.value.as_ref())
            } else {
                None
            }
        })
    }

    fn get_or_default(&self, name: &str) -> String {
        self.get(name).unwrap_or_default().to_string()
    }
}
