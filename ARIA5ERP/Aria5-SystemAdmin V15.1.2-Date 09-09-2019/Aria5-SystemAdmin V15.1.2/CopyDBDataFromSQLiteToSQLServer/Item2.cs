
namespace TopoSortDemo
{
    public class Item2
    {
        public string Name { get; private set; }
        public string[] Dependencies { get; private set; }

        public Item2(string name, params string[] dependencies)
        {
            Name = name;
            Dependencies = dependencies;
        }

        public override string ToString()
        {
            return Name;
        }
    }
}
