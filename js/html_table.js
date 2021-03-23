// Assuming JQuery, or atleast $ = document.querySelectorAll
function parseTableArray(table){
  let map = (arr, fn) => Array.prototype.map.call(arr, fn);
  let header  = map(table.querySelectorAll("tr th"), x => x.innerText);
  let rows = map(table.querySelectorAll("tr"),
                 x => map(x.querySelectorAll("td"), y => y.innerText));
  return [header].concat(rows.filter(x => x));
}
function parseTableObject(table){
  let map = (arr, fn) => Array.prototype.map.call(arr, fn);
  let forEach = (arr, fn) => Array.prototype.forEach.call(arr, fn);
  let header  = map(table.querySelectorAll("tr th"), x => x.innerText);
  let ret = [];
  forEach(table.querySelectorAll("tr"),
          function(tr){
            let tds = tr.querySelectorAll("td");
            if(!tds.length){ return; }
            ret.push(Object.fromEntries(map(tds, (elt,idx) => [header[idx], elt.innerText])))
          }
         )
  return ret;
}
function genTableArray(arr){
  let ce = (x) => document.createElement(x);
  let ctb = (ty, text) => { let node = document.createElement(ty);
                            node.appendChild(document.createTextNode(text));
                            return node; };
  let table = ce("table");
  let header = ce("thead").appendChild(ce("tr"));
  arr[0].forEach(name => header.appendChild(ctb("th", name)));
  table.appendChild(header);
  let tbody = ce("tbody")
  for(let i = 1; i < arr.length; i++){
    let row = ce("tr");
    arr[i].forEach(val => row.appendChild(ctb("td", val)));
    tbody.appendChild(row);
  }
  table.appendChild(tbody);
  return table;
}
function zip(arr1, arr2, op){
  if(!op){
    op = (x,y) => [x,y];
  }
  return arr1.map((elt, idx) => op(elt, arr2[idx]))
}
// parsing battalions table for FE three houses
function genBatttalionTable(){
//  let script = document.createElement("script")
//  script.setAttribute("src", "https://kryogenix.org/code/browser/sorttable/sorttable.js")
//  document.getElementsByTagName("head")[0].appendChild(script);
  let base = parseTableObject(document.querySelector("#base table"));
  let growth = parseTableObject(document.querySelector("#growth table"));
  let stats = ["Phys", "Mag", "Hit", "Crit", "Avo", "Prot", "Res", "Cha"]
  base.forEach(function(b, idx){
    let g = growth[idx];
    stats.map((s) => b[s] = Math.floor(Number(b[s]) + Number(g[s])*4));
  })
  let arr = [Object.keys(base[0])].concat(base.map(Object.values));
  let table = genTableArray(arr);
  table.id = "final";
//  table.setAttribute("class", "sortable")
//  sorttable.makeSortable(table);
  document.getElementById("base").parentElement.appendChild(table);
}
