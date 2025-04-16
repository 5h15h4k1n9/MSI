contract ArrayOps {
    int public size = 5;
    string[] private data = string[] {"", "", "", "", ""};

    function store(int index, string value) public {
        require(index < size, "Index out of bounds");
        data[index] = value;
    }

    function read(int index) public returns (string) {
        require(index < size, "Index out of bounds");
        return data[index];
    }
}
