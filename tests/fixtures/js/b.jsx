/** Some fancy object */
var someCoolObject = {
    /** this has docs! */
    bbba: 1,

    // different docs
    bbb: 2,

    /**
     * Really really really long docs
     * lots of lines and stuff
     * not much too see here
     * <b>asdf</b>
     * `asdf`
     * asdf
     * asdf
     */
    bbbc: 3
};

someCoolObject.bbb

var x = '';

someCoolObject.bbba;

function Data() {
    return <span />;
}

(<div>
    Some text
    {someCoolObject.bbb > 5 ? <Data /> : "text"}
    <span>inside a span</span>
</div>);

class Point {
    constructor(abc, def) {
        this.abc = abc;
        this.def = def;
    }

    toString() {
        return `(${this.abc}, ${this.def})`;
    }
}

// Leave first line blank; cursor will start there
// so that we can complete global scope
/**
 * this is an interesting function
 */
function abc() {

}


export default function xyz(x) {
    return o[x];
}
