(*targetPath=NotebookDirectory[];*)
targetPath="/home/ali/Dropbox/Server/papers/";
triggerDay=1;(*Keep the nature output for the defined number of days between current and publication date*)

authorsList={"Blais","Glazman","Girvin","Schoelkopf","Catelani","DiCarlo","Houck","Schuster","Le Hur","Delsing","Wallraff","Gambetta","Korotkov","Petruccione","Devoret","Manucharyan","Hekking","Guichard","Kerman","Koch","Lukin","Prosen","Cirac","Keeling","Hartmann","Schmidt","Hornberger","Nori","Tureci","Kehrein","Hafezi","Zwolak","T. E. Lee","Knap","Martinis","Jiasen Jin","L. Jiang","Ciuti","Mitra","Wilhelm","Lupascu","C.M. Wilson","C. M. Wilson","Jerry M. Chow","Solano","J. Q. You","A. A. Clerk","Aashish A. Clerk","DiVincenzo","Burkard"};
keywordsList={"circuit QED","artificial atom","superconducting qubit","superconducting circuits","quantum circuits","Jaynes-Cummings","Bose-Hubbard","non-equilibrium phase","nonequilibrium phase","dissipative phase","dynamical phase","non-equilibrium steady state","nonequilibrium steady state","NESS", "non-equilibrium statistical mechanics","nonequilibrium statistical mechanics","open quantum systems" ,"open systems","matrix product operators","Matrix Product states","Keldysh","Dissipative Many-Body","non-Hermitian","Floquet","photon condensate","driven-dissipative","quantum simulators","quantum simulation"};

now=DateList[];
authorsListLen=Length[authorsList];
keywordsListLen=Length[keywordsList];
todayDate=DateString[{"Year","Month","Day"}];
outputHTML=targetPath<>todayDate<>"_paper.html";

(*Functions to extract data from XML*)
extractf[XMLInput_,source_]:=Block[{itemList,titleList,linkList,mainList},
itemList=Cases[XMLInput,XMLElement["item",_,_],Infinity];
If[source=="arXiv",Return[Map[itemExtractarXivf,itemList]]];
If[source=="Nature",Return[Map[itemExtractNatf,itemList]]];
Message[extractf::wrongsource];
Return[];
];
extractf::wrongsource="Unidentified source. Supported source: \"arXiv\", \"Nature\".";

itemExtractarXivf[XMLItem_]:=Block[{title,link,main},
title=Cases[XMLItem,XMLElement["title",_,{ptitle_}]->ptitle,Infinity][[1]];
link=Cases[XMLItem,XMLElement["link",_,{url_}]->url,Infinity][[1]];
main=Cases[XMLItem,XMLElement["description",_,{description_}]->description,Infinity][[1]];
Return[{title,link,main}]
];

itemExtractNatf[XMLItem_]:=Block[{title,jtitle,link,abstract,author,main},
title=Cases[XMLItem,XMLElement["title",_,{ptitle_}]->ptitle,Infinity][[1]];
jtitle=Cases[XMLItem,XMLElement[{_,"publicationName"},_,{journal_}]->journal,Infinity][[1]];
link=Cases[XMLItem,XMLElement[{_,"url"},_,{url_}]->url,Infinity][[1]];
abstract=Cases[XMLItem,XMLElement["description",_,{description_}]->description,Infinity];
author=StringRiffle[Cases[XMLItem,XMLElement[{_,"creator"},_,{creator_}]->creator,Infinity],", "];
title=title<>" ("<>jtitle<>")";
main="<p>Authors: "<>author<>"</p><p>"<>abstract<>"</p>";

(*Publication date check*)
pubDate=DateList[{Cases[XMLItem,XMLElement[{_,"publicationDate"},_,{publicationDate_}]->publicationDate,Infinity][[1]],{"Year","Month","Day"}}];
diffDate=pubDate-now;
If[diffDate[[1]]*(triggerDay+1)+diffDate[[2]]*(triggerDay+1)+diffDate[[3]]>=-1*triggerDay,Return[{title,link,main}],Return[{}]];
];

(*Filtering functions*)
filterf[objectInput_]:=Block[{objectGo,objectGoLen,keepQList,KeepList},
objectGo=objectInput;
objectGoLen=Length[objectGo];

keepQList=Table[AnyTrue[Join[Table[StringContainsQ[ToString[objectGo[[itemid]]],authorsList[[j]]],{j,1,authorsListLen}],Table[StringContainsQ[ToString[objectGo[[itemid]]],keywordsList[[j]]],{j,1,keywordsListLen}]],TrueQ],{itemid,1,objectGoLen}];

KeepList={};
Do[If[keepQList[[itemid]],AppendTo[KeepList,itemid]],{itemid,1,objectGoLen}];
objectGo=objectGo[[KeepList]];
Return[objectGo];
];

(*Generate HTML*)
header="<html><head><title>Filtered summary</title></head><body>";
ending="</body></html>";

HTMLGenf[data_]:=Block[{},
mainText=Map[HTMLEntryf,data];
mainText=StringJoin[mainText];
Return[header<>mainText<>ending];
];

HTMLEntryf[item_]:=Block[{title,main,link},
title="<p class=\"Section\"><font size=\"5\">"<>item[[1]]<>"</font></p>";
main=item[[3]];
link="<p class=\"Text\">"<>"<a href=\""<>item[[2]]<> "\" target=\"_blank\">"<>item[[2]]<>"</a>";
Return[title<>main<>link<>"<hr>"]
];
arXivQPhy=Import["http://export.arxiv.org/rss/quant-ph?version=2.0","XML"];
(*arXivNano=Import["http://export.arxiv.org/rss/cond-mat.mes-hall?version=2.0","XML"];*)
arXivNano=Import["http://export.arxiv.org/rss/cond-mat?version=2.0","XML"];
arXivList={arXivQPhy,arXivNano};arXivListLen=Length[arXivList];
arXivList=Union[filterf[Flatten[Table[extractf[arXivList[[j]],"arXiv"],{j,1,arXivListLen}],1]]];

Nat=Import["http://feeds.nature.com/nature/rss/aop?format=xml","XML"];
NatPhys=Import["http://feeds.nature.com/nphys/rss/aop?format=xml","XML"];
NatPhoton=Import["http://feeds.nature.com/nphoton/rss/aop?format=xml","XML"];
NatMat=Import["http://feeds.nature.com/nmat/rss/aop?format=xml","XML"];
NatList={Nat,NatPhys,NatPhoton,NatMat};NatListLen=Length[NatList];
NatList=Union[filterf[Flatten[Table[extractf[NatList[[j]],"Nature"],{j,1,NatListLen}],1]]];

Export[outputHTML,HTMLGenf[Join[arXivList,NatList]],"Text"];
