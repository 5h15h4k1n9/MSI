contract MappingStorage {
    mapping(int => string) private idToName;

    function read(int id) public returns (string) {
        require(idToName[id] != "Lox", "Not found");
        return idToName[id];
    }

    function store(int id, string name) public {
        idToName[id] = name;
    }
}
