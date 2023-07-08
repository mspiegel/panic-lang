use super::anxious::Anxious;
use super::anxious::Anxious::Nom;

impl From<()> for Anxious<()> {
    fn from(item: ()) -> Self {
        Nom(item)
    }
}
