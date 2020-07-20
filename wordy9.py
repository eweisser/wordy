# Version 7 tried to guess words containing letters that have been established as absent from the user's secret word. It also aimed to simplify existing code, and used the idea of a "gate" that opens once an anagram of the user's word is guessed, allowing the computer to look at the entire lexicon at a time.
# Last version, 8, I plan to get the program to keep track of how many zero-letters are in candidate words, and choose words that have two or three when possible.
# This version, 9, will build off v8's successful targeting of words with letters already ruled out. Specifically, it will assign greater value to letters that are in words containing letters that are already ruled out (obviously, the ruled-out letters should not get points).
# How it chooses what words to guess, and figures out the user's word:
# There's a very small (<1%) chance a random word is chosen.
# Maybe get rid of words that don't exceed .000002 % ? Cf. bewig, aboil, balms, abris, acerb

lexicon = ['abets','abhor','abide','abled','abler','ables','abode','aboil','abort','about','above','abris','abuse','abuts','abysm','acerb','ached','aches','acids','acing','acmes','acnes','acorn','acres','acrid','acted','actin','actor','acute','adept','adieu','adios','adits','admen','admit','admix','adobe','adopt','adore','adorn','adown','adult','adzes','aegis','aeons','afire','afore','afoul','after','agent','agers','agile','agism','agist','aglet','aglow','agone','agony','agues','ahold','aider','aides','ailed','aimed','aimer','aired','airth','aisle','album','alcid','alder','alecs','alefs','aleph','alert','algin','alien','alifs','align','alike','aline','alist','alive','aloes','aloft','alone','along','aloud','alter','altho','altos','alums','amber','ambit','amble','ambos','ambry','amend','amens','ament','amice','amide','amido','amids','amies','amigo','amine','amino','amins','amirs','amity','amnic','amnio','amoks','amole','among','amort','amour','amped','ample','amply','ampul','amuck','amuse','amyls','ancho','andro','angel','anger','angle','anglo','angry','angst','anile','anils','anime','anise','ankhs','ankle','ankus','anode','anole','anomy','anted','antes','antic','antis','antre','antsy','anvil','apers','apery','aphid','aphis','aping','apish','apods','aport','apres','apron','apter','aptly','arced','arcus','ardeb','areic','argil','argle','argol','argon','argot','argue','argus','ariel','arils','arise','armed','arose','arson','artel','artsy','arums','aryls','ascot','asdic','ashed','ashen','aside','asked','asker','askew','askoi','aspen','asper','aspic','aster','astir','atoms','atomy','atone','atony','atopy','atrip','audio','audit','auger','aught','aunts','aunty','aurei','aures','auric','auris','autos','auxin','avers','avert','avion','aviso','avoid','avows','awful','awing','awned','awoke','awols','axels','axile','axils','axing','axiom','axion','axite','axled','axles','axmen','axons','ayins','azide','azido','azine','azlon','azoic','azole','azons','azote','azuki','azure','backs','bacon','badge','badly','bagel','bahts','bails','bairn','baith','baits','baize','baked','baker','bakes','balds','baldy','baled','baler','bales','balks','balky','balms','balmy','banco','bands','bandy','baned','banes','bangs','banjo','banks','banty','bards','bared','bares','barfs','barge','baric','barks','barky','barms','barmy','barns','barny','baron','barye','based','baser','basic','basil','basin','baste','batch','bated','bates','bathe','baths','batik','baton','bauds','baulk','bawds','bawdy','bawls','bawty','bayed','bayou','beach','beads','beady','beaks','beaky','beams','beamy','beano','beans','beard','bears','beast','beats','beaus','beaut','becap','becks','bedim','befit','befog','began','begat','begin','begot','begum','begun','beigy','being','belay','belch','belga','belon','below','belts','bemas','bemix','bench','bends','bendy','bento','bents','bergs','berks','berms','berth','beryl','besom','besot','betas','beths','beton','bevor','bewig','bezil','bhang','bicep','bices','bider','bides','bidet','bield','biers','bight','bigly','bigos','bigot','bijou','biked','biker','bikes','biles','bilge','bilgy','bilks','binal','binds','biner','bines','binge','bingo','bints','biogs','biome','biont','biota','biped','bipod','birch','birds','birks','birle','birls','biros','birse','birth','bison','bitch','biter','bites','bitsy','bizes','black','blade','blahs','blain','blame','blams','bland','blank','blare','blase','blast','blats','blawn','blaws','blaze','bleak','blear','bleat','blend','blent','blest','blets','blimp','blimy','blind','blink','blips','blite','blitz','bloat','block','blocs','blogs','bloke','blond','blots','blown','blows','blowy','blued','bluer','blues','bluet','bluey','blume','blunt','blurs','blurt','blush','board','boars','boast','boats','boche','bocks','bodes','bogan','bogey','bogie','bogle','bogus','bohea','boils','boing','boink','boite','bolar','bolas','bolds','boles','bolts','bolus','bonds','boned','boner','bones','boney','bongs','bonks','bonus','bonze','boral','boras','borax','bored','bores','boric','borks','borne','borts','borty','bortz','bosky','bosun','botas','botch','botel','bothy','bough','boule','bound','bourg','bourn','bouse','bousy','bouts','bovid','bowed','bowel','bower','bowls','bowse','boxed','boxer','boxes','boyar','boyla','brace','brach','bract','brads','braes','brags','braid','brail','brain','brake','braky','brand','brank','brans','brant','brash','brats','brave','bravi','bravo','brawl','brawn','braws','braxy','brays','braze','bread','break','bream','brens','brent','brews','brick','bride','brief','bries','brigs','brims','brine','bring','brink','brins','briny','brios','brisk','brith','brits','broad','brock','broil','broke','brome','bronc','brose','brosy','broth','brown','brows','brugh','bruin','bruit','brume','brung','brunt','brush','brusk','brute','bruts','bucko','bucks','budge','bugle','buhls','buhrs','build','built','bulge','bulgy','bulks','bulky','bumps','bumpy','bunas','bunch','bunco','bunds','bundt','bungs','bunko','bunks','bunts','bunya','buoys','buran','burds','buret','burgh','burgs','burin','burka','burke','burls','burly','burns','burnt','burps','burqa','bursa','burse','burst','bused','bushy','busty','butch','buteo','butes','butle','butyl','buxom','buyer','bylaw','byres','byrls','bytes','caber','cabin','cable','cades','cadet','cadge','cadgy','cadis','cadre','caked','cakes','cakey','calfs','calif','calix','calks','calms','calos','calve','calyx','camel','cameo','cames','camos','campi','campo','camps','campy','candy','caned','caner','canes','canid','canoe','canso','canst','canto','cants','canty','caped','caper','capes','caphs','capiz','capon','capos','caput','carbo','carbs','cards','cared','cares','caret','carex','cargo','carks','carle','carls','carns','carny','carob','carol','carom','carpi','carps','carse','carte','carts','carve','cased','casky','caste','cater','cates','cauld','caulk','cauls','cause','caved','caver','caves','cavie','cavil','cawed','cebid','cedar','cedis','ceiba','ceils','celom','celts','cento','cents','centu','ceorl','ceria','ceros','cesta','cesti','chads','chafe','chain','chair','chais','chalk','champ','chams','chang','chant','chaos','chape','chaps','chapt','chard','chare','chark','charm','chars','chart','chary','chase','chasm','chats','chaws','chays','cheap','cheat','chefs','chela','chemo','chert','chest','chevy','chews','chewy','chiao','chias','chide','chief','chiel','child','chile','chimb','chime','chimp','china','chine','chink','chino','chins','chips','chirk','chirm','chiro','chirp','chiru','chits','chive','chivy','choir','choke','choky','chola','chomp','chops','chord','chore','chose','chows','chubs','chufa','chugs','chump','chums','chunk','churl','churn','chute','chyle','chyme','cibol','cider','cigar','cimex','cines','cions','cires','cited','citer','cites','civet','clade','clads','clags','claim','clamp','clams','clang','clank','clans','claps','clapt','claro','clary','clash','clasp','clast','clave','clavi','claws','clays','clean','clear','cleat','clefs','cleft','clept','clerk','clews','clift','climb','clime','cline','cling','clink','clips','clipt','cloak','clods','clogs','clomb','clomp','clone','clonk','clons','clops','close','cloth','clots','cloud','clour','clout','clove','clown','cloys','cloze','clubs','clued','clues','clump','clung','clunk','cnida','coals','coaly','coapt','coast','coati','coats','cobia','coble','cobra','codas','coden','coder','codes','codex','coeds','coifs','coign','coils','coins','coirs','coked','cokes','colas','colby','colds','coled','coles','colin','colts','comae','comal','comas','combe','combs','comer','comes','comet','comfy','comix','comps','compt','comte','coned','cones','coney','conga','conge','conks','conky','conte','conus','copal','copay','coped','copen','coper','copes','copra','copse','coral','corby','cords','cored','cores','corgi','coria','corks','corky','corms','corns','cornu','corny','corps','corse','coset','cosey','cosie','costa','cotan','coted','cotes','coude','cough','could','count','coupe','coups','court','couth','coved','coven','cover','coves','covet','covey','covin','cowed','cower','cowls','cowry','coxae','coxal','coxed','coxes','coyed','coyer','coypu','cozen','cozes','cozey','cozie','crabs','craft','crags','crake','cramp','crams','crane','crank','crape','craps','crash','crate','crave','crawl','craws','craze','crazy','creak','cream','credo','creds','crept','crepy','crest','crews','cribs','cried','cries','crime','crimp','cripe','crisp','crits','croak','croft','crone','crony','crops','croup','crowd','crown','crows','croze','crude','cruds','cruel','cruet','crumb','crump','cruse','crush','crust','crypt','cubed','cuber','cubes','cubit','cuifs','cuing','cuish','cukes','culet','culex','culms','culpa','culti','cults','cumin','cunts','cupel','cupid','curbs','curds','curdy','cured','cures','curet','curfs','curia','curie','curio','curls','curly','curns','curse','curst','curve','curvy','cushy','cuter','cutes','cutey','cutie','cutin','cutis','cyano','cyans','cyber','cyder','cylix','cymae','cymar','cymas','cymes','cymol','cyton','czars','daces','dagos','dahls','daily','dairy','daisy','dales','dames','damns','damps','dance','dangs','danio','darbs','dares','daric','darks','darns','darts','dashi','dashy','dater','dates','datos','datum','daube','daubs','dauby','daunt','dauts','daven','davit','dawen','dawks','dawns','dawts','dazes','deair','deals','dealt','deans','dears','deary','deash','death','debag','debar','debit','debts','debug','debut','decaf','decal','decay','decks','decor','decos','decoy','decry','defat','defis','defog','degas','degum','deify','deign','deils','deism','deist','deity','delay','delfs','delft','delis','delta','delts','demic','demit','demob','demon','demos','demur','denar','denim','dents','deoxy','depot','depth','derat','deray','derby','derma','derms','detox','devas','devil','devon','dewan','dewar','dewax','dhaks','dhals','dhobi','dhole','dhoti','dhows','dhuti','dials','diary','diazo','dicer','dices','dicey','dicks','dicky','dicot','dicta','dicty','diets','dight','diker','dikes','dikey','dimer','dimes','dimly','dinar','diner','dines','dinge','dingo','dings','dingy','dinks','dinky','dinos','dints','diols','dipso','diram','dirge','dirks','dirls','dirts','dirty','disco','dishy','disme','ditas','ditch','dites','ditsy','ditzy','divan','divas','diver','dives','divot','dizen','djins','doats','dobie','dobla','dobra','docks','doers','doest','doeth','doges','dogey','dogie','dogma','doily','doing','doits','dolce','doles','dolma','dolts','domal','domes','domic','donas','donga','dongs','donsy','donut','dopas','doper','dopes','dopey','dorks','dorky','dorms','dormy','dorps','dorsa','dorty','doser','dotal','doter','dotes','doubt','douce','dough','doula','douma','doums','doura','douse','doven','doves','dowel','dower','downs','downy','dowry','dowse','doxie','doyen','dozen','dozer','dozes','drabs','draft','drags','drail','drain','drake','drams','drank','drape','drats','drave','drawl','drawn','draws','drays','dream','dreck','dregs','dreks','drest','dribs','dries','drift','drily','drink','drips','dript','drive','droit','drone','drops','dropt','drouk','drove','drown','drubs','drugs','drums','drunk','drupe','druse','duals','ducal','ducat','duces','duchy','ducks','ducky','ducts','duels','duets','dukes','dulia','dulse','dumas','dumbo','dumbs','dumka','dumky','dumps','dumpy','dunam','dunce','dunch','dunes','dungs','dungy','dunks','dunts','duomi','duper','dupes','duple','dural','duras','dures','durns','duroc','duros','durst','dusky','dusty','dutch','duvet','dwarf','dwelt','dwine','dyers','dying','dykes','dynel','dynes','earls','early','earns','earth','ebons','ebony','echos','eclat','ecrus','edict','edify','edits','educt','egads','eidos','eight','eikon','eking','elain','eland','elans','elbow','elfin','elint','eloin','email','embar','embay','embow','emirs','emits','empty','emyds','enact','endow','enjoy','enoki','enols','enorm','enows','enrol','ensky','entia','entry','envoi','envoy','enzym','eosin','epact','ephod','epics','epoch','epoxy','equal','equid','equip','ergot','erica','eruct','erupt','escot','estop','ethic','ethos','ethyl','etnas','etuis','etyma','euros','evict','evils','exact','exalt','exams','exing','exist','exits','exons','expat','expos','extol','extra','exult','exurb','eying','eyras','fable','faced','facer','faces','facet','facts','fader','fades','fadge','fados','faery','fagin','fagot','fails','faint','fairs','fairy','faith','faked','faker','fakes','fakey','fakir','false','famed','fames','fancy','fanes','fangs','fanos','fanum','faqir','farce','farci','farcy','fards','fared','fares','farle','farls','farms','farts','fated','fates','fatly','fatso','faugh','fauld','fault','fauns','fauve','faves','favor','favus','fawns','fawny','faxed','faxes','fayed','fazed','fazes','fears','feast','feats','fecal','fecks','feign','feint','feist','felid','felon','felts','femur','fends','feods','feral','feria','ferly','fermi','ferns','ferny','fetal','fetas','fetch','fetid','fetor','fetus','feuar','feuds','fiats','fiber','fibre','fices','fiche','fichu','ficus','fidge','fidos','field','fiend','fiery','fight','filar','filch','filed','filer','files','filet','films','filmy','filos','filth','filum','final','finca','finch','finds','fined','finer','fines','finks','finos','fiord','fique','fired','fires','firms','firns','first','firth','fishy','fitch','fitly','fiver','fives','fixed','fixer','fixes','fjeld','fjord','flabs','flack','flags','flair','flake','flaky','flame','flams','flamy','flank','flans','flaps','flare','flash','flask','flats','flaws','flawy','flaxy','flays','fleas','fleck','flesh','flews','fleys','flick','flics','flied','flier','flies','fling','flint','flips','flirs','flirt','flite','flits','float','flock','flocs','floes','flogs','flops','flora','flota','flour','flout','flown','flows','flubs','flued','flues','fluid','fluke','fluky','flume','flump','flung','flunk','fluor','flush','flute','fluty','fluyt','flyer','flyte','foals','foams','foamy','focal','focus','foehn','fogey','fogie','fohns','foils','foist','folds','foley','folia','folic','folks','folky','fonds','fondu','fonts','foram','foray','forbs','forby','force','fords','fores','forge','forks','forky','forme','forms','forte','forth','forts','forty','forum','fouls','found','fount','fours','fovea','fowls','foxed','foxes','foyer','frags','frail','frame','franc','frank','fraps','frats','fraud','frays','freak','fremd','frena','fresh','frets','fried','fries','frigs','frise','frisk','frith','frits','fritz','frock','froes','frogs','frond','frons','front','frosh','frost','froth','frown','frows','froze','frugs','fruit','frump','fubar','fucks','fudge','fuels','fugal','fugio','fugle','fujis','fumed','fumer','fumes','fumet','fundi','funds','fungi','fungo','funks','funky','furan','furls','furze','fused','fusel','fusil','fusty','futon','fuzed','fuzes','fuzil','gable','gadis','gadje','gadjo','gaily','gains','gaits','gales','galop','gambe','gambs','gamed','gamer','games','gamey','gamic','gamin','gamps','gamut','ganef','ganev','ganof','gaols','gaped','gaper','gapes','garbs','garni','garth','gated','gater','gates','gator','gauds','gaudy','gault','gaums','gaunt','gaurs','gauze','gauzy','gavel','gavot','gawks','gawky','gawps','gawsy','gayer','gazed','gazer','gazes','gears','gecko','gecks','gelds','gelid','gelts','gemot','genic','genip','genoa','genom','genro','gents','genua','genus','geoid','gerah','germs','germy','getas','getup','geums','ghast','ghats','ghaut','ghazi','ghost','ghoul','giant','gibed','giber','gibes','gifts','gilds','gilts','gimel','gimps','gimpy','ginks','ginzo','gipon','gipsy','girds','girls','girly','girns','giron','giros','girsh','girth','girts','gismo','gites','given','giver','gives','gizmo','glace','glade','glads','glady','glair','glams','gland','glans','glare','glary','glaze','glazy','gleam','glean','gleba','gleds','glens','gleys','glias','glide','glime','glims','glint','glitz','gloam','gloat','globe','globs','gloms','glops','glory','glost','glout','glove','glows','gloze','glued','gluer','glues','gluey','glume','glums','gluon','glute','gluts','glyph','gnarl','gnars','gnash','gnats','gnaws','gnome','goads','goals','goats','goban','godet','godly','goers','gofer','golds','golem','golfs','gomer','gonad','gonef','goner','gonia','goral','gored','gores','gorms','gorps','gorse','gorsy','goths','gourd','gouts','gouty','gowan','gowds','gowks','gowns','goxes','goyim','grabs','grace','grade','grads','graft','grail','grain','gramp','grams','grand','grans','grant','grape','graph','grapy','grasp','grate','grave','gravy','grays','graze','great','greys','gride','grids','grief','grift','grime','grimy','grind','grins','griot','gripe','grips','gript','gripy','grist','grith','grits','groan','groat','grody','groin','groks','grope','grosz','grots','group','grout','grove','growl','grown','grows','grubs','gruel','grues','grume','grump','grunt','guaco','guano','guans','guard','guars','gucks','gudes','guest','guide','guids','guild','guile','guilt','guiro','guise','gular','gulch','gules','gulfs','gulfy','gulps','gulpy','gumbo','gunks','gunky','gursh','gushy','gusto','gusty','gutsy','guyed','guyot','gwine','gybed','gybes','gyoza','gyral','gyred','gyres','gyron','gyros','gyrus','gyved','gyves','habit','habus','hacek','hacks','hades','hadji','hadst','haems','haets','hafiz','hafts','haiks','haiku','hails','haint','hairs','hairy','hajes','hajis','hakes','hakim','haled','haler','hales','halid','halms','halon','halos','halts','halve','hames','hance','hands','handy','hangs','hanks','hanky','hanse','hants','haole','haply','hards','hardy','hared','harem','hares','harks','harls','harms','harps','harpy','harts','haste','hasty','hated','hater','hates','haulm','hauls','haunt','haute','haven','haver','haves','havoc','hawed','hawks','hawse','hayed','hayer','hazed','hazel','hazer','hazes','heads','heady','heals','heaps','heapy','heard','hears','heart','heats','heavy','hecks','hedgy','hefts','hefty','heils','heirs','heist','helio','helix','helms','helos','helot','helps','hemal','hemic','hemin','hemps','hempy','henry','hents','herbs','herby','herds','herls','herma','herms','herns','heron','heros','hertz','hexad','hexyl','hicks','hider','hides','hijab','hijra','hiked','hiker','hikes','hilar','hilts','hilum','hilus','hinds','hinge','hinky','hints','hiply','hired','hires','hived','hives','hoagy','hoard','hoars','hoary','hocks','hocus','hoers','hogan','hoick','hoise','hoist','hoked','hokes','hokey','hokum','holds','holed','holes','holey','holks','holms','holts','homed','homer','homes','homey','homie','honda','honed','honer','hones','honey','hongi','hongs','honks','honky','hoped','hoper','hopes','horal','horas','horde','horns','horny','horse','horst','horsy','hosed','hosel','hosen','hoser','hosey','hosta','hotel','hotly','hound','houri','hours','house','hovel','hover','howdy','howes','howfs','howks','howls','hoyas','hoyle','hucks','huger','hulas','hulks','hulky','human','humic','humid','humor','humps','humpy','hunks','hunky','hunts','hurds','hurls','hurly','hurst','hurts','husky','hydra','hydro','hyena','hying','hylas','hymen','hymns','hyoid','hyped','hyper','hypes','hypos','hyrax','hyson','iambs','ichor','icker','icons','ictus','ideal','ideas','idler','idles','idols','idyls','iglus','ihram','ikats','ikons','ileac','ileum','ileus','image','imago','imbed','imbue','imped','impel','imply','inapt','inarm','inbye','incog','incur','incus','index','indol','indow','indue','inept','inert','infer','infos','infra','ingle','ingot','inked','inker','inkle','inlay','inlet','input','inset','inter','intro','inure','invar','iotas','irade','irate','irked','irone','irons','irony','isled','islet','istle','itchy','items','ither','ivory','ixora','ixtle','izars','jabot','jacks','jacky','jades','jager','jails','jakes','jalop','jambe','jambs','janes','janty','japed','japer','japes','jarls','jatos','jauks','jaunt','jaups','jawed','jeans','jehad','jehus','jerid','jerks','jerky','jeton','jibed','jiber','jibes','jihad','jilts','jimpy','jingo','jinks','jived','jiver','jives','jivey','jocks','joeys','johns','joins','joint','joist','joked','joker','jokes','jokey','joles','jolts','jolty','jones','joram','jorum','jotas','joual','jouks','joule','joust','jowar','jowed','jowls','jowly','joyed','jubas','jubes','jucos','judas','judge','judos','jugal','juice','juicy','juked','jukes','julep','jumbo','jumps','jumpy','junco','junks','junky','junta','junto','jupes','jupon','jural','jurat','jurel','jutes','kadis','kafir','kaifs','kails','kains','kales','kalif','kames','kanes','kanji','kanzu','kaons','kaphs','kaput','karns','karst','karts','kauri','kaury','kayos','kbars','kebar','kefir','keirs','kelim','kelps','kelpy','kelts','kemps','kempt','kenaf','kench','kendo','kenos','kepis','kerbs','kerfs','kerns','ketch','ketol','kevil','khadi','khafs','khans','khats','kheda','khets','khoum','kiang','kibes','kibla','kiefs','kiers','kilns','kilos','kilts','kilty','kinas','kinds','kines','kings','kinos','kirns','kited','kiter','kites','kithe','kiths','kivas','klong','kluge','klutz','knaps','knars','knaur','knave','knawe','knead','knelt','knife','knish','knits','knobs','knops','knosp','knots','knout','knows','knurl','knurs','koans','koels','kohls','koine','kojis','kolas','kombu','kophs','kopje','korai','koras','korat','korma','korun','kraft','krait','kraut','krona','krone','kudos','kufis','kugel','kumys','kurta','kyars','kyats','kylix','kyrie','kytes','kythe','labor','laced','lacer','laces','lacey','lacks','laden','lader','lades','laevo','lager','laich','laics','laigh','laird','lairs','laith','laity','laked','laker','lakes','lakhs','lambs','lamby','lamed','lamer','lames','lamps','lance','lands','lanes','lanky','lapin','lapis','lapse','larch','lards','lardy','lares','large','largo','laris','larks','larky','larum','lased','laser','latch','lated','laten','later','latex','lathe','laths','lathy','latke','lauds','laugh','laved','laver','laves','lawed','lawns','lawny','laxer','laxes','layed','layer','layin','layup','lazed','lazes','leach','leads','leady','leafs','leafy','leaks','leaky','leans','leant','leaps','leapt','learn','lears','leary','leash','least','leavy','ledgy','lefts','lefty','legit','lehrs','lehua','leman','lemon','lemur','lends','lenis','lenos','lento','lepta','lesbo','letch','letup','leuds','levin','levis','lewis','lexis','liane','liang','liard','liars','liber','libra','licht','licks','lidar','lidos','liens','liers','lieus','lifer','lifts','ligan','liger','light','liked','liken','liker','likes','liman','limas','limba','limbo','limbs','limby','limed','limen','limes','limey','limns','limos','limpa','limps','linac','lindy','lined','liner','lines','liney','linga','lingo','lings','lingy','links','linky','linos','lints','linty','linum','lions','liras','litas','liter','lithe','litho','litre','lived','liven','liver','lives','livre','loach','loads','loafs','loams','loamy','loans','loath','lobar','lobed','lobes','lochs','locks','locum','locus','loden','lodes','lodge','lofts','lofty','logan','loges','logia','logic','login','loids','loins','loner','longe','longs','loped','loper','lopes','loran','lords','lores','loris','loser','lotah','lotas','lotic','lotus','lough','louie','louis','louma','loupe','loups','lours','loury','louse','lousy','louts','lovat','loved','lover','loves','lowed','lower','lowes','lowse','loxed','loxes','lubed','lubes','luces','lucid','lucks','lucky','lucre','ludes','ludic','luged','luger','luges','lumas','lumen','lumps','lumpy','lunar','lunas','lunch','lunes','lunet','lunge','lungi','lungs','lunks','lunts','lupin','lurch','lured','lures','lurex','lurid','lurks','lusty','lutea','luted','lutes','luxes','lweis','lyard','lyart','lyase','lycea','lycra','lying','lymph','lynch','lyres','lyric','lysed','lysin','lytic','mabes','maced','macer','maces','mache','macho','machs','macks','macle','macon','macro','madly','madre','mafic','mages','magic','magot','magus','mahoe','maids','maile','mails','mains','mairs','maist','maize','major','maker','makes','makos','males','malic','malts','malty','maned','manes','mange','mango','mangy','manic','manly','manor','manos','manse','manus','maple','maqui','march','marcs','mares','marge','marks','marls','marly','marse','marsh','marts','marvy','maser','mashy','mason','match','mated','mater','mates','matey','maths','matin','matzo','mauds','mauls','maund','mauts','mauve','maven','mavie','mavin','mavis','mawed','maxed','maxes','maxis','maybe','mayed','mayor','mayos','mayst','mazed','mazer','mazes','mbira','meads','meals','mealy','means','meant','meany','meats','meaty','medal','media','medic','meiny','melds','melic','melon','melts','melty','menad','mends','mensa','mensh','menta','menus','meous','meows','merch','mercs','mercy','merit','merks','merls','meshy','mesic','meson','metal','meths','metis','metol','metro','mewls','miaou','miaow','miaul','micas','miche','micks','micra','micro','midge','midst','miens','might','miked','mikes','milch','milds','miler','miles','milks','milky','milos','milpa','milts','milty','minae','minas','mince','mincy','minds','mined','miner','mines','mingy','minke','minks','minor','mints','minty','minus','mired','mires','mirex','mirks','mirky','mirth','mirza','misdo','miser','misty','miter','mites','mitre','mixed','mixer','mixes','mixup','mizen','moans','moats','mocha','mocks','modal','model','modes','modus','mogul','mohel','mohur','moils','moira','moire','moist','mokes','molar','molas','molds','moldy','moles','molts','monad','monas','monde','money','monie','monks','monte','month','moped','moper','mopes','mopey','morae','moral','moras','moray','morel','mores','morns','morph','morse','morts','mosey','moste','motel','motes','motey','moths','mothy','motif','mouch','moues','mould','moult','mound','mount','mourn','mouse','mousy','mouth','moved','mover','moves','movie','mowed','mower','moxie','mucho','mucid','mucin','mucks','mucky','mucor','mucro','mudra','mufti','muhly','mujik','mulch','mulct','muled','mules','muley','munch','mungo','munis','muons','mural','muras','mured','mures','murex','murid','murks','murky','musca','mused','muser','mushy','music','musky','musth','musty','mutch','muted','muter','mutes','muton','mylar','mynah','mynas','myoid','myope','mysid','myths','nabes','nabis','nacho','nacre','nadir','naifs','nails','nairu','naive','naked','naled','named','namer','names','napes','narco','narcs','nards','nares','naric','naris','narks','narky','nasty','natch','nates','navel','naves','nazis','neaps','nears','neath','neats','necks','negus','neifs','neigh','neist','nemas','nerds','nerdy','nerol','nerts','nertz','nervy','netop','neuks','neums','nevus','newly','newsy','newts','nexus','nicad','nicer','niche','nicks','nicol','nidal','nides','nidus','nifty','nighs','night','nipas','niter','nites','nitre','nitro','nival','nixed','nixes','nizam','noble','nobly','nocks','nodal','nodes','nodus','noels','noils','noily','noirs','noise','noisy','nomad','nomas','nomes','nopal','noria','noris','norms','north','nosed','nosey','notal','notch','noted','noter','notes','notum','novae','novas','novel','noway','nowts','nubia','nucha','nuder','nudes','nudge','nudie','nuked','nukes','numbs','nurls','nurse','nutsy','nymph','oaken','oakum','oared','oaten','oater','oaths','oaves','obeah','obeli','obeys','obias','obits','objet','ocean','ocher','ochre','ochry','ocker','ocrea','octad','octal','octan','octyl','oculi','odahs','odeum','odist','odium','odyle','odyls','ofays','often','ofter','ogams','ogham','ogive','ogled','ogler','ogles','ogres','ohias','ohing','ohmic','oiled','oiler','oinks','okapi','okays','okehs','okras','olden','older','oldie','oleic','olein','oleum','olive','omber','ombre','omega','omens','omers','omits','oncet','onery','onium','onlay','onset','ontic','opahs','opals','opens','opera','opine','oping','opium','opsin','opted','optic','orach','orals','orang','orate','orbed','orbit','orcas','orcin','oread','organ','orgic','oriel','orles','ornis','orpin','osier','osmic','ostia','other','ought','ounce','ouphe','ouphs','ourie','ousel','outby','outed','outer','outre','ouzel','ovals','ovary','ovate','ovens','overs','overt','ovine','ovule','owing','owlet','owned','owner','owsen','oxide','oxids','oxime','oxims','oxlip','oxter','oyers','paced','pacer','paces','pacey','packs','pacts','padis','padle','padre','padri','paeon','paged','pager','pages','pagod','paiks','pails','pains','paint','pairs','paise','paled','paler','pales','palet','palms','palmy','palsy','pandy','paned','panel','panes','pangs','panic','pansy','panto','pants','panty','parch','pardi','pards','pardy','pared','pareo','pares','pareu','parge','pargo','paris','parks','parle','parol','parse','parts','party','parve','parvo','paseo','paste','pasty','patch','pated','paten','pater','pates','paths','patin','patio','patly','patsy','pause','paved','paver','paves','pavid','pavin','pavis','pawed','pawer','pawky','pawls','pawns','paxes','payed','payer','payor','peach','peags','peaks','peaky','peals','peans','pearl','pears','peart','peats','peaty','peavy','pecan','pechs','pecks','pecky','pedal','pedro','peins','pekan','pekin','pelfs','pelon','pelts','penal','pends','pengo','penis','peons','peony','perch','perdu','perdy','peril','peris','perks','perky','perms','pervs','pesky','pesto','pesty','petal','pewit','phage','phase','phial','phlox','phone','phons','phony','phots','phuts','phyla','phyle','piano','pians','pical','picas','picks','picky','picot','picul','piers','pieta','piety','pigmy','pikas','piked','piker','pikes','pilaf','pilar','pilau','pilaw','pilea','piled','piles','pilot','pilus','pimas','pinas','pinch','pined','pines','piney','pingo','pings','pinko','pinks','pinky','pinot','pinta','pinto','pints','pions','pious','pique','pirns','pirog','pisco','piste','pitas','pitch','piths','pithy','piton','pivot','pixel','pixes','place','plack','plage','plaid','plain','plait','plane','plank','plans','plant','plash','plasm','plate','plats','platy','plays','plead','pleas','pleat','plebs','plena','pleon','plews','plica','plied','plier','plies','plink','plods','plonk','plots','plotz','plows','ploys','pluck','plugs','plumb','plume','plums','plumy','plunk','plush','plyer','poach','pocks','pocky','podgy','podia','poems','poesy','poets','pogey','poilu','poind','point','poise','poked','poker','pokes','pokey','polar','poled','poler','poles','polis','polka','polys','pomes','ponce','ponds','pones','pongs','porch','pored','pores','porgy','porks','porky','porns','porny','ports','posed','poser','posit','potsy','pouch','poufs','poult','pound','pours','pouts','pouty','power','poxed','poxes','prahu','prams','prang','prank','praos','prase','prate','prats','praus','prawn','prays','presa','prest','prexy','preys','price','prick','pricy','pride','pried','pries','prigs','prima','prime','primo','prims','prink','print','prion','prise','prism','privy','prize','proas','probe','prods','proem','profs','progs','prole','proms','prone','prong','prose','prost','prosy','proud','prove','prowl','prows','proxy','prude','prune','pruta','psalm','pseud','pshaw','psoae','psych','pubes','pubic','pubis','puces','pucka','pucks','pudgy','pudic','pujah','pujas','puked','pukes','puled','puler','pules','pulik','pulis','pulse','pumas','punas','punch','pungs','punji','punka','punks','punky','punto','punts','punty','purda','purge','purin','puris','purls','purse','pursy','purty','pushy','puton','pyins','pylon','pyoid','pyran','pyres','pyrex','pyric','pyros','pyxes','pyxie','pyxis','qadis','qaids','qophs','quack','quads','quags','quail','quais','quake','quaky','quale','qualm','quark','quart','quash','quasi','quate','quays','qubit','quean','quern','query','quest','queys','quick','quids','quiet','quilt','quins','quint','quips','quire','quirk','quirt','quite','quits','quods','quoin','quoit','quota','quote','quoth','qursh','rabic','rabid','raced','races','racks','racon','radio','radix','radon','rafts','raged','rages','ragis','raids','rails','rains','rainy','raise','rajes','raked','rakes','rakis','rakus','rales','ralph','ramen','ramet','ramie','ramps','ramus','rance','ranch','rands','randy','range','rangy','ranid','ranis','ranks','rants','raped','rapes','raphe','rapid','rased','raspy','ratch','rated','ratel','rates','rathe','ratio','ratos','raved','ravel','raven','raves','ravin','rawin','rawly','raxed','raxes','rayed','rayon','razed','razes','reach','react','reads','ready','realm','reals','reams','reaps','rebid','rebop','rebus','rebut','rebuy','recap','recit','recks','recon','recta','recti','recto','recut','redan','redia','redip','redly','redon','redos','redox','redub','redux','refit','refix','refly','regal','regma','regna','rehab','reifs','reify','reign','reink','reins','rejig','relax','relay','relic','relit','reman','remap','remit','remix','renal','rends','renig','rents','reoil','repay','repin','reply','repos','repot','resat','resaw','resay','resid','resin','resit','resod','resow','retag','retax','retch','retia','rewan','rewax','rewin','rewon','rheas','rheum','rhino','rhomb','rhumb','rhyme','rhyta','rials','riant','ribes','riced','rices','ricks','rides','ridge','ridgy','riels','rifle','rifts','right','riled','riles','riley','rimed','rimes','rinds','rindy','rings','rinks','rinse','rioja','riots','riped','ripen','ripes','risen','risky','rites','ritzy','rival','rived','riven','rives','rivet','riyal','roach','roads','roams','roans','roast','robed','robes','robin','roble','rocks','rocky','rodes','rogue','roils','roily','roles','rolfs','roman','romps','roped','ropes','ropey','roque','rosed','roset','roshi','rosin','rotas','rotch','rotes','rotis','rotls','rouen','roues','rouge','rough','round','roups','roupy','rouse','roust','route','routh','routs','roved','roven','roves','rowan','rowdy','rowed','rowel','rowen','rowth','royal','rubel','rubes','ruble','ruche','rucks','rugae','rugal','rugby','ruing','ruins','ruled','rules','rumba','rumen','rumps','runes','rungs','runic','runts','runty','rushy','rusty','ruths','rutin','ryked','rykes','rynds','ryots','sabed','saber','sabin','sabir','sable','sabot','sabre','sadhe','sadhu','sadly','safer','sager','sagum','sahib','saice','saint','saith','sajou','saker','salep','salic','salmi','salon','salty','salve','salvo','sambo','samek','sandy','saned','saner','sangh','santo','sapid','sapor','sarge','sargo','sarin','sarky','sarod','sated','satem','satin','satyr','sauce','sauch','saucy','saugh','sault','saury','saute','saved','saver','savin','savor','savoy','sawed','sawer','sayed','sayer','sayid','scald','scale','scalp','scaly','scamp','scant','scape','scare','scarf','scarp','scart','scary','scaup','scaur','scena','scend','scent','schav','schmo','schul','schwa','scion','scold','scone','scope','score','scorn','scour','scout','scowl','scrag','scram','scrap','screw','scrim','scrip','scrod','scrub','scrum','scuba','scudi','scudo','sculk','sculp','scurf','scuta','scute','seamy','sebum','sedan','sedgy','sedum','segni','segno','selah','selva','sengi','senor','senti','sepal','sepia','sepic','sepoy','septa','serac','serai','seral','serif','serin','serow','serum','servo','setal','seton','setup','sewan','sewar','sexto','shack','shade','shady','shaft','shake','shako','shaky','shale','shalt','shaly','shame','shank','shape','shard','share','shark','sharn','sharp','shaul','shave','shawl','shawm','shawn','sheaf','sheal','shear','sheik','shelf','shend','shent','sheol','sherd','shewn','shied','shiel','shier','shift','shily','shine','shiny','shire','shirk','shirt','shiva','shive','shlep','shlub','shoal','shoat','shock','shoed','shoer','shogi','shoji','shone','shore','shorl','shorn','short','shote','shout','shove','shown','showy','shred','shrew','shrub','shrug','shtik','shuck','shuln','shunt','shute','shyer','sibyl','sicko','sidhe','sidle','sieur','sight','sigla','sigma','signa','siker','silex','silky','silty','silva','simar','since','sinew','singe','siped','sired','siren','sirup','sitar','sited','situp','siver','sixmo','sixte','sixth','sixty','sizar','sized','sizer','skald','skate','skean','skein','skelm','skelp','skied','skier','skiey','skimo','skimp','skint','skirl','skirt','skite','skive','skoal','skort','skyed','slack','slain','slake','slang','slank','slant','slate','slaty','slave','slept','slice','slick','slide','slier','slime','slimy','sling','slink','slipe','slipt','sloid','slojd','slope','sloth','sloyd','slued','slump','slung','slunk','slurb','slurp','slyer','slype','smack','smalt','smart','smaze','smear','smelt','smerk','smile','smirk','smite','smith','smock','smoke','smoky','smolt','smote','snack','snafu','snail','snake','snaky','snare','snarf','snark','snarl','snath','sneak','sneap','sneck','snick','snide','snipe','snore','snort','snout','snowy','snuck','soapy','soave','sober','socle','sodic','sofar','softa','softy','solan','solar','soldi','soled','solei','solid','solum','solve','soman','sonar','sonde','sonic','sonly','sophy','sored','sorel','sorta','sough','sound','soupy','south','sowar','sowed','sower','soyuz','space','spacy','spade','spado','spaed','spahi','spail','spait','spake','spale','spang','spank','spare','spark','spate','spawn','speak','spean','spear','speck','speil','speir','spelt','spend','spent','sperm','spica','spice','spick','spicy','spied','spiel','spier','spike','spiky','spile','spilt','spine','spiny','spire','spirt','spiry','spite','spitz','splat','splay','split','spode','spoil','spoke','spore','sport','spout','sprag','sprat','spray','sprig','sprit','sprue','sprug','spued','spume','spumy','spunk','spurn','spurt','sputa','squab','squad','squat','squaw','squeg','squib','squid','stack','stade','stage','stagy','staid','staig','stain','stair','stake','stale','stalk','stamp','stand','stane','stang','stank','staph','stare','stark','stave','stead','steak','steal','steam','stein','stela','steno','stern','stewy','stich','stick','stied','stile','stime','stimy','sting','stink','stipe','stirk','stirp','stoae','stoai','stock','stogy','stoic','stoke','stole','stoma','stomp','stone','stony','stope','store','stork','storm','story','stoup','stour','stove','stowp','strap','straw','stray','strep','strew','stria','strip','strop','strow','stroy','strum','stuck','study','stump','stung','stunk','stupa','stupe','styed','style','styli','suave','subah','suber','sucky','sucre','sudor','suety','sugar','suing','suint','suite','sulfa','sulfo','sulky','sumac','super','supra','surah','sural','surfy','surge','surgy','surly','sutra','swage','swail','swain','swale','swami','swamp','swamy','swang','swank','sward','sware','swarf','swarm','swart','swath','swear','sweat','swept','swift','swine','swing','swink','swipe','swirl','swith','swive','sword','swore','sworn','swoun','swung','sylph','sylva','synch','synod','synth','syren','syrup','taber','tabes','tabid','table','tabor','tabun','tabus','taces','tache','tachs','tacks','tacky','tacos','taels','tahrs','tails','tains','taken','taker','takes','takin','talcs','taler','tales','talks','talky','talon','taluk','talus','tamed','tamer','tames','tamis','tamps','tango','tangs','tangy','tanks','tansy','taped','taper','tapes','tapir','tapis','tardo','tardy','tared','tares','targe','tarns','taros','tarps','tarsi','tauon','taupe','tawed','tawer','tawie','tawny','tawse','taxed','taxer','taxes','taxis','taxol','taxon','taxus','teach','teaks','teals','teams','tears','teary','techs','techy','tegua','teind','telco','telia','telic','teloi','telos','tempi','tempo','temps','tench','tends','tendu','tenia','tenor','tepal','tepas','tepid','tepoy','terai','terga','terms','terns','tesla','teuch','teugh','texas','thack','thane','thank','tharm','thaws','theca','thegn','thein','their','thens','therm','thesp','thews','thewy','thick','thief','thine','thing','think','thins','thiol','third','thirl','thole','thong','thorn','thorp','those','thous','thraw','threw','thrip','throb','throe','throw','thrum','thuds','thugs','thuja','thumb','thump','thunk','thurl','thuya','thyme','thymi','tical','ticks','tidal','tides','tiers','tiger','tigon','tikes','tilak','tilde','tiled','tiler','tiles','timed','timer','times','tinea','tined','tines','tinge','tings','tipsy','tired','tires','tirls','tiros','toads','toady','today','toeas','tofus','togae','togas','togue','toile','toils','tokay','toked','token','toker','tokes','tolan','tolar','tolas','toled','toles','tolus','toman','tombs','tomes','tonal','tondi','toned','toner','tones','toney','tonga','tongs','tonic','tonus','topaz','toped','toper','topes','tophe','tophi','tophs','topic','topis','toque','torah','toras','torch','torcs','tores','toric','torse','torsi','torsk','torus','touch','tough','tours','touse','towed','towel','tower','towie','towns','towny','toxic','toxin','toyed','toyer','trace','track','trade','tragi','traik','trail','train','tramp','trams','trank','tranq','trans','traps','trash','trave','trawl','trays','tread','treks','trend','trews','treys','triac','triad','trial','tribe','trice','trick','tried','tries','trigo','trigs','trike','trims','trine','triol','trios','tripe','trips','troak','trock','trode','trogs','trois','troke','tromp','trona','trone','trope','trove','trows','troys','truce','truck','trued','trues','trugs','truly','trump','trunk','tryma','tsked','tubae','tubal','tubas','tubed','tuber','tubes','tucks','tufas','tules','tulip','tumid','tumor','tumps','tunas','tuned','tuner','tunes','tungs','tunic','tupik','turbo','turds','turfs','turfy','turks','turns','turps','tushy','tuxes','tuyer','twaes','twain','twang','tweak','twerp','twice','twier','twigs','twine','twins','twiny','twirl','twirp','tyers','tying','tykes','tyned','tynes','typal','typed','types','typic','typos','tyred','tyres','tyros','tzars','udons','uhlan','ukase','ulans','ulcer','ulema','ulnad','ulnae','ulnar','ulnas','ulpan','ultra','ulvas','umbel','umber','umbos','umbra','umiac','umiak','umiaq','umped','unais','unapt','unarm','unary','unbar','unbid','unbox','uncap','uncia','uncle','uncos','uncoy','under','unfed','unfit','unfix','ungot','unhat','unhip','unify','unite','units','unity','unjam','unlay','unled','unlet','unlit','unmet','unmew','unmix','unpeg','unrig','unrip','unsay','unset','unsew','unsex','untie','until','unwed','unwet','unwit','unzip','upbow','upbye','updos','updry','upend','uplit','upset','uraei','urase','urate','urban','urbia','ureal','ureas','uredo','ureic','urged','urges','urial','urine','urped','ursid','usage','usher','using','usnea','uteri','utile','uveal','uveas','vague','vagus','vails','vairs','vakil','vales','valet','valid','valor','valse','value','vamps','vampy','vaned','vanes','vangs','vapid','vapor','varix','varus','vatic','vatus','vault','vaunt','veals','vealy','vegan','veils','veins','veiny','velar','velds','veldt','velum','venal','vends','venom','vents','venus','verbs','verso','verst','verts','vertu','vesta','vetch','vexil','vials','viand','vibes','vicar','viced','vices','vichy','video','viers','views','viewy','vigas','vigor','viler','vimen','vinal','vinas','vinca','vined','vines','vinos','vinyl','viola','viols','viper','viral','vireo','vires','virga','virls','virtu','virus','vised','visor','vista','vitae','vital','vixen','vizor','vocab','vocal','voces','vodka','vodun','vogue','voice','voids','voila','voile','volar','voled','voles','volta','volte','volti','volts','vomer','vomit','voted','voter','votes','vouch','vowed','vowel','vower','vrouw','vulgo','vying','wacke','wacko','wacks','wacky','wader','wades','wadis','wafer','wafts','waged','wager','wages','wagon','waifs','wails','wains','wairs','waist','waits','waive','waked','waken','waker','wakes','waled','waler','wales','walks','waltz','wames','wamus','wands','waned','wanes','waney','wanks','wanly','wants','wards','wared','wares','warks','warms','warns','warps','warts','warty','washy','waspy','waste','watch','water','waugh','wauks','wauls','waved','waver','waves','wavey','waxed','waxen','waxer','waxes','weald','weals','weans','wears','weary','wecht','wedgy','wefts','weigh','weird','weirs','wekas','welch','welds','welsh','welts','wench','wends','wetly','whack','whale','whamo','whams','whang','whaps','wharf','whats','whaup','wheal','wheat','whelk','whelm','whelp','whens','whets','wheys','whids','whigs','while','whims','whine','whins','whiny','whips','whipt','whirl','whirs','whisk','whist','white','whits','whity','whole','whomp','whops','whore','whorl','whort','whose','whump','whups','wicks','widen','wider','wides','width','wield','wifed','wifes','wifey','wifty','wigan','wight','wilco','wilds','wiled','wiles','wilts','wimps','wimpy','wince','winch','winds','windy','wined','wines','winey','wings','wingy','winks','winos','winze','wiped','wiper','wipes','wired','wires','wised','wiser','wisha','wispy','witan','witch','wited','wites','withe','withy','wived','wiver','wives','wizen','wizes','woads','woald','wodge','woful','woken','wolds','wolfs','woman','wombs','womby','women','womyn','wonks','wonky','wonts','words','wordy','works','world','worms','wormy','worse','worst','worth','worts','would','wound','woven','wrack','wrang','wraps','wrapt','wrath','wreak','wreck','wrens','wrest','wrick','wried','wries','wring','wrist','write','writs','wrong','wrote','wroth','wrung','wurst','wyled','wyles','wynds','wyted','wytes','xenia','xenic','xeric','xerus','xylan','xylem','xysti','yacht','yacks','yager','yagis','yaird','yamen','yamun','yangs','yanks','yards','yarns','yawed','yawls','yawns','yawps','yclad','yeahs','yeans','yearn','years','yeast','yelps','yenta','yerba','yerks','yetis','yeuks','yield','yikes','yince','yipes','yirds','yirth','ylems','yocks','yodel','yodhs','yodle','yogas','yoghs','yogic','yogin','yogis','yoked','yokel','yokes','yolks','yores','young','yours','youse','youth','yowie','yowls','yuans','yucas','yucks','yules','yurts','zaire','zarfs','zaxes','zayin','zeals','zebra','zebus','zeins','zerks','zeros','zesty','zetas','zibet','zilch','zincs','zincy','zineb','zines','zings','zingy','zinky','ziram','zlote','zloty','zoeal','zoeas','zombi','zonae','zonal','zoned','zoner','zones','zonks','zoril','zoris','zouks','zowie']
preservedLexicon = []           # a preserved lexicon is necessary because the computer deletes words from the original lexicon
for i in range(len(lexicon)):   # the user's needs to be able to guess any word in the full lexicon, however
    preservedLexicon.append(lexicon[i])

from random import random # There are len(lexicon) (5749 last time I checked) words in the lexicon. They'll be #0 to #5748
import math
import time     # probably can delete this import once all the testing's done
print ("Pick a secret five-letter word! Once you have it, try to figure out mine!")
compSecWord = lexicon[math.floor(random()*len(lexicon))]
currentUserGuess = "hi"
#print (compSecWord + "   ...that's my secret word.") # That's the secret word.
print ("You can go first.")
turnNumber = 1
aScore = 1    # All letters start with a score of 1. Each time the computer guesses a word and gets feedback from the user,
bScore = 1    # a relevant letter's score is multiplied by a value LESS THAN ZERO. So, unguessed letters have the highest letter scores.
cScore = 1
dScore = 1
eScore = 1
fScore = 1
gScore = 1
hScore = 1
iScore = 1
jScore = 1
kScore = 1
lScore = 1
mScore = 1
nScore = 1
oScore = 1
pScore = 1
qScore = 1
rScore = 1
sScore = 1
tScore = 1
uScore = 1
vScore = 1
wScore = 1
xScore = 1
yScore = 1
zScore = 1
lettersNotYetChecked = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
lettersRuledOut = []
lettersRuledIn = []
lexiconSectionSize = 1000
shouldBeDeletedFromLex = []

def userTakesAGuess():
    currentUserLettersCorrect = 0
    #print ("What's your guess?")
    #currentUserGuess = input()     # this is the real line, greened while building the program for efficiency
    currentUserGuess = "cloud"      # dummy value for efficiency; deleted this later
    found = False
    for z in range(len(preservedLexicon)):    # making sure the user's guess is an acceptable word to guess
        if currentUserGuess == preservedLexicon[z]:
            found = True    # yes, the user's guess IS in the lexicon
    if found == False:      # if the user's guess is NOT in the lexicon...
        if currentUserGuess == "q":
            print (">>>Bye!")
            exit()
        else:
            print ("Sorry, that's not in my lexicon. Try again.")
            userTakesAGuess()
    else:
        for x in currentUserGuess:
            for y in compSecWord:
                if x == y:
                    currentUserLettersCorrect = currentUserLettersCorrect + 1
        if currentUserGuess == compSecWord:
            print ("Well done! That's my secret word!")
        #else:
            #if currentUserLettersCorrect == 1:
                #print ("You have " + str(currentUserLettersCorrect) + " letter correct.")
            #else:
                #print ("You have " + str(currentUserLettersCorrect) + " letters correct.")



def compTakesAGuess():
    global turnNumber
    global aScore
    global bScore
    global cScore
    global dScore
    global eScore
    global fScore
    global gScore
    global hScore
    global iScore
    global jScore
    global kScore
    global lScore
    global mScore
    global nScore
    global oScore
    global pScore
    global qScore
    global rScore
    global sScore
    global tScore
    global uScore
    global vScore
    global wScore
    global xScore
    global yScore
    global zScore
    global lettersNotYetChecked
    global lettersRuledOut
    global lettersRuledIn
    global lexiconSectionSize

    shouldBeDeletedFromLex.clear()
    for x in range(len(lexicon)): # lexicon "maintenance" or "pruning"
        wordZeroCount = 0
        wordDefYesCount = 0
        for y in range(5):
            if lexicon[x][y] in lettersRuledOut:
                wordZeroCount = wordZeroCount + 1
            if lexicon[x][y] in lettersRuledIn:
                wordDefYesCount += 1
        if len(lettersRuledIn) == 5:    # The computer has figured out all five letters.
            if wordDefYesCount < 5:     # Time to rule out any word that doesn't have all of them.
                shouldBeDeletedFromLex.append(lexicon[x])
                #print (lexicon[x].upper() + " will be deleted from the lexicon.")
        else:   # The computer has NOT figured out all five letters. Proceed...
            if wordZeroCount == 5:
                shouldBeDeletedFromLex.append(lexicon[x])
                #print (lexicon[x].upper() + " will be deleted from the lexicon.")
            if wordZeroCount == 4 and wordDefYesCount == 1:
                shouldBeDeletedFromLex.append(lexicon[x])
                #print (lexicon[x].upper() + " will be deleted from the lexicon--4 out/1 in.")
            if wordZeroCount == 3 and wordDefYesCount == 2:
                shouldBeDeletedFromLex.append(lexicon[x])
                #print (lexicon[x].upper() + " will be deleted from the lexicon--3 out/2 in.")
            if wordZeroCount == 2 and wordDefYesCount == 3:
                shouldBeDeletedFromLex.append(lexicon[x])
                #print (lexicon[x].upper() + " will be deleted from the lexicon--2 out/3 in.")
            if wordZeroCount == 1 and wordDefYesCount == 4:
                shouldBeDeletedFromLex.append(lexicon[x])
                #print (lexicon[x].upper() + " will be deleted from the lexicon--1 out/4 in.")

    #print (len(lexicon))
    for x in range(len(shouldBeDeletedFromLex)):
        #print (shouldBeDeletedFromLex[x])
        lexicon.remove(shouldBeDeletedFromLex[x])   # this is the command that actually deletes the words from the lexicon
    print (str(len(lexicon)) + " words are left in the lexicon.")

    # end of lexicon maintenance
    # calculating scores for different candidate words:

    candidateList = []
    duplicateCandidateList = []
    candidateMultiplyScoreList = []
    duplicateMultiplyScoreList = []
    candidateAddScoreList = []
    duplicateAddScoreList = []
    candidateZeroCountList = []
    for z in range(lexiconSectionSize): # don't look at every word in the lexicon, just get 1000
        candidateCompGuess = lexicon[math.floor(random()*len(lexicon))]
        candidateList.append(candidateCompGuess)    # "candidateList" is a list of all the words the computer MIGHT guess this round; it'll compare all of them
        duplicateCandidateList.append(candidateCompGuess)    # duplicating it
        candidateMultiplyScore = 1      # before evaluation, every word starts with an M-score of 1, and an A-score of 0.
        candidateAddScore = 0
        if "a" in candidateCompGuess:
            candidateMultiplyScore *= aScore
            candidateAddScore += aScore
        if "b" in candidateCompGuess:
            candidateMultiplyScore *= bScore
            candidateAddScore += bScore
        if "c" in candidateCompGuess:
            candidateMultiplyScore *= cScore
            candidateAddScore += cScore
        if "d" in candidateCompGuess:
            candidateMultiplyScore *= dScore
            candidateAddScore += dScore
        if "e" in candidateCompGuess:
            candidateMultiplyScore *= eScore
            candidateAddScore += eScore
        if "f" in candidateCompGuess:
            candidateMultiplyScore *= fScore
            candidateAddScore += fScore
        if "g" in candidateCompGuess:
            candidateMultiplyScore *= gScore
            candidateAddScore += gScore
        if "h" in candidateCompGuess:
            candidateMultiplyScore *= hScore
            candidateAddScore += hScore
        if "i" in candidateCompGuess:
            candidateMultiplyScore *= iScore
            candidateAddScore += iScore
        if "j" in candidateCompGuess:
            candidateMultiplyScore *= jScore
            candidateAddScore += jScore
        if "k" in candidateCompGuess:
            candidateMultiplyScore *= kScore
            candidateAddScore += kScore
        if "l" in candidateCompGuess:
            candidateMultiplyScore *= lScore
            candidateAddScore += lScore
        if "m" in candidateCompGuess:
            candidateMultiplyScore *= mScore
            candidateAddScore += mScore
        if "n" in candidateCompGuess:
            candidateMultiplyScore *= nScore
            candidateAddScore += nScore
        if "o" in candidateCompGuess:
            candidateMultiplyScore *= oScore
            candidateAddScore += oScore
        if "p" in candidateCompGuess:
            candidateMultiplyScore *= pScore
            candidateAddScore += pScore
        if "q" in candidateCompGuess:
            candidateMultiplyScore *= qScore
            candidateAddScore += qScore
        if "r" in candidateCompGuess:
            candidateMultiplyScore *= rScore
            candidateAddScore += rScore
        if "s" in candidateCompGuess:
            candidateMultiplyScore *= sScore
            candidateAddScore += sScore
        if "t" in candidateCompGuess:
            candidateMultiplyScore *= tScore
            candidateAddScore += tScore
        if "u" in candidateCompGuess:
            candidateMultiplyScore *= uScore
            candidateAddScore += uScore
        if "v" in candidateCompGuess:
            candidateMultiplyScore *= vScore
            candidateAddScore += vScore
        if "w" in candidateCompGuess:
            candidateMultiplyScore *= wScore
            candidateAddScore += wScore
        if "x" in candidateCompGuess:
            candidateMultiplyScore *= xScore
            candidateAddScore += xScore
        if "y" in candidateCompGuess:
            candidateMultiplyScore *= yScore
            candidateAddScore += yScore
        if "z" in candidateCompGuess:
            candidateMultiplyScore *= zScore
            candidateAddScore += zScore
        candidateMultiplyScoreList.append(candidateMultiplyScore)   # making a list of the multiply scores for each candidate word
        duplicateMultiplyScoreList.append(candidateMultiplyScore)
        candidateAddScoreList.append(candidateAddScore)     # making a list of the addition scores for each candidate word
        candidateZeroCount = 0
        for x in range(5):
            if candidateCompGuess[x] in lettersRuledOut:
                candidateZeroCount = candidateZeroCount + 1
        candidateZeroCountList.append(candidateZeroCount)
            
    # Which word will the computer use?:

    if random() < 0.0000001 and turnNumber > 9:     # A very small chance to pick a random word; basically a placeholder in case I want to increase the odds later
        currentCompGuess = lexicon[math.floor(random()*len(lexicon))] # comp picks a random word

    else:   # almost always do this instead of random choice:

        if random() < turnNumber / 60:	# choose (M) maximum-scoring word (multiplicative score)
                                        # the probability of choosing this method increases with time (by turn 30, 50% chance)
            currentCompGuess = candidateList[candidateMultiplyScoreList.index(max(candidateMultiplyScoreList))]
            print ("! Method: Pick word with greatest multiplicative score.")
        elif turnNumber > 3 and random() < 0.9666666666667 and max(candidateZeroCountList) > 0:
            if 4 in candidateZeroCountList:
                #candidateZeroCountList.index(4))) is index of first occurrence of 4 in Zero Count List.
                currentCompGuess = candidateList[candidateZeroCountList.index(4)]
            elif 3 in candidateZeroCountList:
                #candidateZeroCountList.index(3))) is index of first occurrence of 3 in Zero Count List.
                currentCompGuess = candidateList[candidateZeroCountList.index(3)]
            elif 2 in candidateZeroCountList:
                #candidateZeroCountList.index(2))) is index of first occurrence of 2 in Zero Count List.
                currentCompGuess = candidateList[candidateZeroCountList.index(2)]
            elif 1 in candidateZeroCountList:
                #candidateZeroCountList.index(1))) is index of first occurrence of 1 in Zero Count List.
                currentCompGuess = candidateList[candidateZeroCountList.index(1)]
        else:   #try to choose a word with an M-score of 0 but a fairly high A-score.
            duplicateAddScoreList = candidateAddScoreList # delete me later!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            maxAddIndex = candidateAddScoreList.index(max(candidateAddScoreList))

            while duplicateMultiplyScoreList[duplicateAddScoreList.index(max(duplicateAddScoreList))] != 0:
                duplicateCandidateList.remove(duplicateCandidateList[maxAddIndex])  #eliminating any A-max-scoring word that doesn't have an M-score of 0
                duplicateAddScoreList.pop(maxAddIndex)
                duplicateMultiplyScoreList.pop(maxAddIndex)
                if len(duplicateCandidateList) == 0:    #if the list is down to 0, there are no words with an M-score of 0. Stop.
                    break
                maxAddIndex = duplicateAddScoreList.index(max(duplicateAddScoreList)) #find the new max, now that the old max has been eliminated
            
            if turnNumber == 1:     # For the first turn, there won't be any user feedback yet, so we need an exception
                currentCompGuess = lexicon[math.floor(random()*len(lexicon))] # comp picks a random word
            elif len(duplicateCandidateList) > 0:   # if the comp DID find a word with an M-score of 0
                currentCompGuess = duplicateCandidateList[duplicateAddScoreList.index(max(duplicateAddScoreList))]
            else:   # if the comp did NOT find a word with an M-score of 0
                currentCompGuess = candidateList[candidateMultiplyScoreList.index(max(candidateMultiplyScoreList))]

    if turnNumber % 10 == 1 and turnNumber != 11:
        print ("My " + str(turnNumber) + "st guess is " + currentCompGuess.upper() + ". How many letters do I have right?")
    elif turnNumber % 10 == 2 and turnNumber != 12:
        print ("My " + str(turnNumber) + "nd guess is " + currentCompGuess.upper() + ". How many letters do I have right?")
    elif turnNumber % 10 == 3 and turnNumber != 13:
        print ("My " + str(turnNumber) + "rd guess is " + currentCompGuess.upper() + ". How many letters do I have right?")
    else:
        print ("My " + str(turnNumber) + "th guess is " + currentCompGuess.upper() + ". How many letters do I have right?")
    lexicon.remove(currentCompGuess)

    for i in range(5):
        if currentCompGuess[i] in lettersNotYetChecked:
            lettersNotYetChecked.remove(currentCompGuess[i])

    print ("Not yet checked: " + str(lettersNotYetChecked))
    print ("Letters ruled out: " + str(lettersRuledOut))
    print ("Letters ruled in: " + str(lettersRuledIn))

    currentUserFeedback = input()
    if currentUserFeedback in ["0","1","2","3","4","5"]:
        currentUserFeedback = int(currentUserFeedback)
    elif currentUserFeedback == "q":
        print (">>> Bye!")
        exit()
    else:
        print ("Sorry, that doesn't make sense!")
    if currentUserFeedback == 0:    # The user said the computer's latest guess had 0 correct letters.
        for x in range(5):
            if currentCompGuess[x] not in lettersRuledOut:
                lettersRuledOut.append(currentCompGuess[x])     # this adds each letter of a word with a reply of 0 to the "letters ruled out" list

            if currentCompGuess[x] == "a":
                aScore *= 0
            elif currentCompGuess[x] == "b":
                bScore *= 0
            elif currentCompGuess[x] == "c":
                cScore *= 0
            elif currentCompGuess[x] == "d":
                dScore *= 0
            elif currentCompGuess[x] == "e":
                eScore *= 0
            elif currentCompGuess[x] == "f":
                fScore *= 0
            elif currentCompGuess[x] == "g":
                gScore *= 0
            elif currentCompGuess[x] == "h":
                hScore *= 0
            elif currentCompGuess[x] == "i":
                iScore *= 0
            elif currentCompGuess[x] == "j":
                jScore *= 0
            elif currentCompGuess[x] == "k":
                kScore *= 0
            elif currentCompGuess[x] == "l":
                lScore *= 0
            elif currentCompGuess[x] == "m":
                mScore *= 0
            elif currentCompGuess[x] == "n":
                nScore *= 0
            elif currentCompGuess[x] == "o":
                oScore *= 0
            elif currentCompGuess[x] == "p":
                pScore *= 0
            elif currentCompGuess[x] == "q":
                qScore *= 0
            elif currentCompGuess[x] == "r":
                rScore *= 0
            elif currentCompGuess[x] == "s":
                sScore *= 0
            elif currentCompGuess[x] == "t":
                tScore *= 0
            elif currentCompGuess[x] == "u":
                uScore *= 0
            elif currentCompGuess[x] == "v":
                vScore *= 0
            elif currentCompGuess[x] == "w":
                wScore *= 0
            elif currentCompGuess[x] == "x":
                xScore *= 0
            elif currentCompGuess[x] == "y":
                yScore *= 0
            elif currentCompGuess[x] == "z":
                zScore *= 0
            
    elif currentUserFeedback == 1:      # The user said the computer's latest guess had 1 correct letter.
        thisWordsLettersRuledOut = 0
        for x in range(5):
            if currentCompGuess[x] in lettersRuledOut:
                thisWordsLettersRuledOut += 1

            if currentCompGuess[x] == "a":
                aScore *= 0.2
            elif currentCompGuess[x] == "b":
                bScore *= 0.2
            elif currentCompGuess[x] == "c":
                cScore *= 0.2
            elif currentCompGuess[x] == "d":
                dScore *= 0.2
            elif currentCompGuess[x] == "e":
                eScore *= 0.2
            elif currentCompGuess[x] == "f":
                fScore *= 0.2
            elif currentCompGuess[x] == "g":
                gScore *= 0.2
            elif currentCompGuess[x] == "h":
                hScore *= 0.2
            elif currentCompGuess[x] == "i":
                iScore *= 0.2
            elif currentCompGuess[x] == "j":
                jScore *= 0.2
            elif currentCompGuess[x] == "k":
                kScore *= 0.2
            elif currentCompGuess[x] == "l":
                lScore *= 0.2
            elif currentCompGuess[x] == "m":
                mScore *= 0.2
            elif currentCompGuess[x] == "n":
                nScore *= 0.2
            elif currentCompGuess[x] == "o":
                oScore *= 0.2
            elif currentCompGuess[x] == "p":
                pScore *= 0.2
            elif currentCompGuess[x] == "q":
                qScore *= 0.2
            elif currentCompGuess[x] == "r":
                rScore *= 0.2
            elif currentCompGuess[x] == "s":
                sScore *= 0.2
            elif currentCompGuess[x] == "t":
                tScore *= 0.2
            elif currentCompGuess[x] == "u":
                uScore *= 0.2
            elif currentCompGuess[x] == "v":
                vScore *= 0.2
            elif currentCompGuess[x] == "w":
                wScore *= 0.2
            elif currentCompGuess[x] == "x":
                xScore *= 0.2
            elif currentCompGuess[x] == "y":
                yScore *= 0.2
            elif currentCompGuess[x] == "z":
                zScore *= 0.2

        if thisWordsLettersRuledOut == 4:
            for x in range(5):
                if currentCompGuess[x] not in lettersRuledOut and currentCompGuess[x] not in lettersRuledIn:
                    lettersRuledIn.append(currentCompGuess[x])
            

    elif currentUserFeedback == 2:  # The user said the computer's latest guess had 2 correct letters.
        for x in range(5):
            if currentCompGuess[x] == "a":
                aScore *= 0.4
            elif currentCompGuess[x] == "b":
                bScore *= 0.4
            elif currentCompGuess[x] == "c":
                cScore *= 0.4
            elif currentCompGuess[x] == "d":
                dScore *= 0.4
            elif currentCompGuess[x] == "e":
                eScore *= 0.4
            elif currentCompGuess[x] == "f":
                fScore *= 0.4
            elif currentCompGuess[x] == "g":
                gScore *= 0.4
            elif currentCompGuess[x] == "h":
                hScore *= 0.4
            elif currentCompGuess[x] == "i":
                iScore *= 0.4
            elif currentCompGuess[x] == "j":
                jScore *= 0.4
            elif currentCompGuess[x] == "k":
                kScore *= 0.4
            elif currentCompGuess[x] == "l":
                lScore *= 0.4
            elif currentCompGuess[x] == "m":
                mScore *= 0.4
            elif currentCompGuess[x] == "n":
                nScore *= 0.4
            elif currentCompGuess[x] == "o":
                oScore *= 0.4
            elif currentCompGuess[x] == "p":
                pScore *= 0.4
            elif currentCompGuess[x] == "q":
                qScore *= 0.4
            elif currentCompGuess[x] == "r":
                rScore *= 0.4
            elif currentCompGuess[x] == "s":
                sScore *= 0.4
            elif currentCompGuess[x] == "t":
                tScore *= 0.4
            elif currentCompGuess[x] == "u":
                uScore *= 0.4
            elif currentCompGuess[x] == "v":
                vScore *= 0.4
            elif currentCompGuess[x] == "w":
                wScore *= 0.4
            elif currentCompGuess[x] == "x":
                xScore *= 0.4
            elif currentCompGuess[x] == "y":
                yScore *= 0.4
            elif currentCompGuess[x] == "z":
                zScore *= 0.4
    elif currentUserFeedback == 3:
        for x in range(5):
            if currentCompGuess[x] == "a":
                aScore *= 0.6
            elif currentCompGuess[x] == "b":
                bScore *= 0.6
            elif currentCompGuess[x] == "c":
                cScore *= 0.6
            elif currentCompGuess[x] == "d":
                dScore *= 0.6
            elif currentCompGuess[x] == "e":
                eScore *= 0.6
            elif currentCompGuess[x] == "f":
                fScore *= 0.6
            elif currentCompGuess[x] == "g":
                gScore *= 0.6
            elif currentCompGuess[x] == "h":
                hScore *= 0.6
            elif currentCompGuess[x] == "i":
                iScore *= 0.6
            elif currentCompGuess[x] == "j":
                jScore *= 0.6
            elif currentCompGuess[x] == "k":
                kScore *= 0.6
            elif currentCompGuess[x] == "l":
                lScore *= 0.6
            elif currentCompGuess[x] == "m":
                mScore *= 0.6
            elif currentCompGuess[x] == "n":
                nScore *= 0.6
            elif currentCompGuess[x] == "o":
                oScore *= 0.6
            elif currentCompGuess[x] == "p":
                pScore *= 0.6
            elif currentCompGuess[x] == "q":
                qScore *= 0.6
            elif currentCompGuess[x] == "r":
                rScore *= 0.6
            elif currentCompGuess[x] == "s":
                sScore *= 0.6
            elif currentCompGuess[x] == "t":
                tScore *= 0.6
            elif currentCompGuess[x] == "u":
                uScore *= 0.6
            elif currentCompGuess[x] == "v":
                vScore *= 0.6
            elif currentCompGuess[x] == "w":
                wScore *= 0.6
            elif currentCompGuess[x] == "x":
                xScore *= 0.6
            elif currentCompGuess[x] == "y":
                yScore *= 0.6
            elif currentCompGuess[x] == "z":
                zScore *= 0.6
    elif currentUserFeedback == 4:
        for x in range(5):
            if currentCompGuess[x] == "a":
                aScore *= 0.8
            elif currentCompGuess[x] == "b":
                bScore *= 0.8
            elif currentCompGuess[x] == "c":
                cScore *= 0.8
            elif currentCompGuess[x] == "d":
                dScore *= 0.8
            elif currentCompGuess[x] == "e":
                eScore *= 0.8
            elif currentCompGuess[x] == "f":
                fScore *= 0.8
            elif currentCompGuess[x] == "g":
                gScore *= 0.8
            elif currentCompGuess[x] == "h":
                hScore *= 0.8
            elif currentCompGuess[x] == "i":
                iScore *= 0.8
            elif currentCompGuess[x] == "j":
                jScore *= 0.8
            elif currentCompGuess[x] == "k":
                kScore *= 0.8
            elif currentCompGuess[x] == "l":
                lScore *= 0.8
            elif currentCompGuess[x] == "m":
                mScore *= 0.8
            elif currentCompGuess[x] == "n":
                nScore *= 0.8
            elif currentCompGuess[x] == "o":
                oScore *= 0.8
            elif currentCompGuess[x] == "p":
                pScore *= 0.8
            elif currentCompGuess[x] == "q":
                qScore *= 0.8
            elif currentCompGuess[x] == "r":
                rScore *= 0.8
            elif currentCompGuess[x] == "s":
                sScore *= 0.8
            elif currentCompGuess[x] == "t":
                tScore *= 0.8
            elif currentCompGuess[x] == "u":
                uScore *= 0.8
            elif currentCompGuess[x] == "v":
                vScore *= 0.8
            elif currentCompGuess[x] == "w":
                wScore *= 0.8
            elif currentCompGuess[x] == "x":
                xScore *= 0.8
            elif currentCompGuess[x] == "y":
                yScore *= 0.8
            elif currentCompGuess[x] == "z":
                zScore *= 0.8
    elif currentUserFeedback == 5:
        print ("Did I guess your word?")
        computerSuccessYesOrNo = input()
        if computerSuccessYesOrNo[0].lower() == "y":
            print ("Yay! I enjoyed that.")
            exit()
        lexiconSectionSize = len(lexicon)
        for x in range(5):
            if currentCompGuess[x] not in lettersRuledIn:
                lettersRuledIn.append(currentCompGuess[x])
            if currentCompGuess[x] == "a":
                aScore *= 2.5
            elif currentCompGuess[x] == "b":
                bScore *= 2.5
            elif currentCompGuess[x] == "c":
                cScore *= 2.5
            elif currentCompGuess[x] == "d":
                dScore *= 2.5
            elif currentCompGuess[x] == "e":
                eScore *= 2.5
            elif currentCompGuess[x] == "f":
                fScore *= 2.5
            elif currentCompGuess[x] == "g":
                gScore *= 2.5
            elif currentCompGuess[x] == "h":
                hScore *= 2.5
            elif currentCompGuess[x] == "i":
                iScore *= 2.5
            elif currentCompGuess[x] == "j":
                jScore *= 2.5
            elif currentCompGuess[x] == "k":
                kScore *= 2.5
            elif currentCompGuess[x] == "l":
                lScore *= 2.5
            elif currentCompGuess[x] == "m":
                mScore *= 2.5
            elif currentCompGuess[x] == "n":
                nScore *= 2.5
            elif currentCompGuess[x] == "o":
                oScore *= 2.5
            elif currentCompGuess[x] == "p":
                pScore *= 2.5
            elif currentCompGuess[x] == "q":
                qScore *= 2.5
            elif currentCompGuess[x] == "r":
                rScore *= 2.5
            elif currentCompGuess[x] == "s":
                sScore *= 2.5
            elif currentCompGuess[x] == "t":
                tScore *= 2.5
            elif currentCompGuess[x] == "u":
                uScore *= 2.5
            elif currentCompGuess[x] == "v":
                vScore *= 2.5
            elif currentCompGuess[x] == "w":
                wScore *= 2.5
            elif currentCompGuess[x] == "x":
                xScore *= 2.5
            elif currentCompGuess[x] == "y":
                yScore *= 2.5
            elif currentCompGuess[x] == "z":
                zScore *= 2.5
    

while currentUserGuess != compSecWord:
    userTakesAGuess()
    compTakesAGuess()
    turnNumber += 1