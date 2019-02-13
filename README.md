# Fietsparkeerinzichten analysedashboard

De code in deze github is geschreven om een R shiny analysedashboard voor het gebruik van bewaakte fietsenstallingen inzichtelijk te maken. De code bevat specifieke documentatie voor het hergebruik. Het doel was om onderstaande vragen te beantwoorden

# Vraagstelling om te beantwoorden met het dashboard

Het hogere doel is dat we een gezonde stad willen zijn, fietsen stimuleren draagt daar aan bij, waar weer voldoende stallingsplekken voor nodig zijn.

1.	Is het aantal stallingsplekken in de stallingen voldoende? Hier hangt een aantal subvragen onder:
  *	Hoe vaak is een stalling vol? Op welke tijden/dagen zit een stalling vol?
    *	Wat is het aantal bezette plekken per stalling? 
Ik vind het lastig of dat een gemiddelde per uur moet zijn, of dat dat een grafiek moet worden met het aantal vrije plekken door de dag. Ik denk dat we aan het tweede meer hebben, omdat een gemiddelde per uur minder zegt.
En dan zou het prettig zijn om naast het aantal bezette plekken ook het aandeel bezette plekken te kunnen zien.
    *	Idem voor het omgekeerde: het aantal en aandeel vrije plekken per stalling.
  *	Hoe vaak is een stalling bijna vol (ik ken geen definitie van bijna vol, maar laten we uitgaan van nog 10% vrije plekken?)?
2.	Hoe lang staan fietsen gestald per stalling? Ik denk dan aan: hoeveel procent staat er korter dan een uur, hoeveel staat er tussen de 1 en de 4 uur, hoeveel staat er tussen de 4 en de 12 uur, hoeveel tussen de 12 en de 24 uur en hoeveel langer dan 24 uur)? En hoe is dat door de week, is daar iets over te zeggen?
3.	Wat is het aantal in- en uitgaande stallers/fietsen per uur per stalling?
En dan in de spitsen liefst per kwartier om te zien wanneer de in- en uitstroom het grootst zijn.
Dat geeft een beeld van de drukte/piekmomenten. En ik zou dat ook (tzt) af willen zetten tegen drukte op de fietspaden. We tellen continu hoeveel fietsers er rijden op de fietspaden aan weerszijden van de busbaan Vredenburg. En sinds kort ook hoeveel fietsers er rijden door de Van Sijpesteijntunnel.
4.	Is het aantal stallingsplekken op straat voldoende? Dus: hoe vaak komt het voor dat er meer fietsen staan dan er plekken zijn? En waar/wanneer?
5.	Een vraag die we op basis van de data willen beantwoorden: wat doen fietsers op het moment dat hun voorkeursstalling (bijna) vol is? Gaan ze naar een andere stalling? Zetten ze fiets op straat?
6.	En een andere vraag: is het aantal gestalde fietsen te voorspellen? Welke indicatoren zijn daarvoor nodig? Moment van de dag, het weer, …?
