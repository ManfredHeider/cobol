      *****************************************************************
      *                                                               *
      *    COPY-MEMBER: DRAT0009.CPY                                 
      *    STANDARD-KONSTANTEN                                        *
      *                                                               *
      *             >>>>>>> Hinweis beachten <<<<<<<<                 *
      *                                                               *
      *                                                               *
      *  In der Vergangenheit haben wir in einigen Fällen Probleme    *
      *  mit neu eingefügten Konstanten bekommen. Dabei hat sich      *
      *  herausgestellt, daß vorhandene Programme diese Konstanten    *
      *  in der WKS definiert hatten und die Umwandlung nun nicht     *
      *  nicht mehr korrekt lief.                                     *
      *  Aus dieser Erfahrung wurde nun beschlossen:                  *
      *                                                               *
      *  1.) Vor dem Einfügen einer Konstante, ist in allen Programmen*
      *      nach dieser Konstante zu suchen, z.B. mit FileAid        *
      *                                                               *
      *  2.) Der Entwickler hat die Konsistenz aller vorhandenen      *
      *      Programme sicherzustellen.                               *
      *      Betroffene Programme sind entsprechend anzupassen.       *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * TLMAE-695  !09.02.22! DEFINITION K-ZUGRIFF-MAX     ! HEIDER   *
      * -----------!--------!------------------------------!----------
      * DBO-10293  !14.09.22! NEUES FELD: K-NI-MINUS-5     ! T13AC20
      * -----------!--------!------------------------------!----------
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************

       01  FILLER.
      *   POSITIONIERUNG CURSOR AUF FELD
           05  K-CURSOR             PIC S9(004) COMP VALUE -1.
      *   KEIN NULL-WERT IN NULL-INDICATOR
      *    VORSICHT:
      *       K-NOT-NULL DARF NICHT ZUR ABFRAGE AUF NULL-FELDER
      *                  GESETZT WERDEN, SONDERN NUR ZUM SETZEN
      *                  DES NULL-INDIKATORS AUF NOT-NULL
           05  K-NOT-NULL           PIC S9(004) COMP VALUE  0.
      *   NULL-WERT IN NULL-INDICATOR
           05  K-NULL               PIC S9(004) COMP VALUE -1.
      *   WERT -5 IN NULL-INDICATOR
           05  K-NI-MINUS-5         PIC S9(004) COMP VALUE -5.
      *   FELD HELL ERLEUCHTEN
           05  K-ATTR               PIC  X(001)     VALUE 'H'.
      *   MELDUNGS-TYP, DER ZU BLINKENDER ANZEIGE FUEHRT
           05  K-MELD-TYP-BLINK     PIC  X(001)     VALUE 'B'.
      *
      *   MAX. WIEDERHOLUNG BEI RESOURCE NICHT VERFÜGBAR/DEADLOCK
090222*    05  K-ZUGRIFF-MAX        PIC S9(004) PACKED-DECIMAL
090222     05  K-ZUGRIFF-MAX        PIC S9(003) PACKED-DECIMAL
                                                VALUE 5.
      *
      *   ÜBERGABEPARAMETER FÜR STANDARD-ABBRUCHROUTINE 'ILBOABN0'
           05  K-COMP-CODE          PIC S9(004) BINARY VALUE +3501.
      *   ÜBERGABEPARAMETER FÜR NEUE ABBRUCHROUTINE CEE3ABD
           05  K-CEE3ABD-CODE       PIC S9(009) BINARY VALUE +3501.
           05  K-CEE3ABD-TIMING     PIC S9(009) BINARY VALUE 1.
      *   ABBRUCHROUTINE
           05  K-CEE3ABD            PIC  X(8) VALUE 'CEE3ABD'.
      *   KENNZEICHNUNG FüR EIN NICHT GEFüLLTES PRODUKTFELD
           05  K-KEIN-PRODUKT       PIC S9(04)  COMP VALUE -1.
      *   KENNZEICHNUNG FUER EINEN UNGUELTIGEN PRODUKTSCHLUESSEL
           05  K-PRODUKT-UNGUELTIG  PIC S9(04)  COMP VALUE +0998.
      *
      *   MAXIMALE ANZAHL VON ERGEBNISZEILEN AUS EINEM LIST-OZP
      *   (UNBEGRENZTE ANZAHL WÜRDE SYSTEM ÜBERLASTEN)
      *    05  K-MAX-ERG-OZP        PIC S9(04)  COMP VALUE +1000.
           05  K-MAX-ERG-OZP        PIC S9(09)  COMP VALUE +32000.
      *
      *   NUMERISCHE KONSTANTEN
       01  FILLER.
           05  K-ZERO               PIC  9(001)     VALUE 0.
           05  K-1                  PIC  9(001)     VALUE 1.
           05  K-2                  PIC  9(001)     VALUE 2.
           05  K-3                  PIC  9(001)     VALUE 3.
           05  K-4                  PIC  9(001)     VALUE 4.
           05  K-5                  PIC  9(001)     VALUE 5.
           05  K-6                  PIC  9(001)     VALUE 6.
           05  K-7                  PIC  9(001)     VALUE 7.
           05  K-8                  PIC  9(001)     VALUE 8.
           05  K-9                  PIC  9(001)     VALUE 9.
           05  K-10                 PIC  9(002)     VALUE 10.
           05  K-11                 PIC  9(002)     VALUE 11.
           05  K-12                 PIC  9(002)     VALUE 12.
           05  K-13                 PIC  9(002)     VALUE 13.
           05  K-14                 PIC  9(002)     VALUE 14.
           05  K-15                 PIC  9(002)     VALUE 15.
           05  K-16                 PIC  9(002)     VALUE 16.
           05  K-17                 PIC  9(002)     VALUE 17.
           05  K-18                 PIC  9(002)     VALUE 18.
           05  K-19                 PIC  9(002)     VALUE 19.
           05  K-20                 PIC  9(002)     VALUE 20.
           05  K-21                 PIC  9(002)     VALUE 21.
           05  K-22                 PIC  9(002)     VALUE 22.
           05  K-23                 PIC  9(002)     VALUE 23.
           05  K-24                 PIC  9(002)     VALUE 24.
           05  K-25                 PIC  9(002)     VALUE 25.
           05  K-26                 PIC  9(002)     VALUE 26.
           05  K-27                 PIC  9(002)     VALUE 27.
           05  K-28                 PIC  9(002)     VALUE 28.
           05  K-29                 PIC  9(002)     VALUE 29.
           05  K-30                 PIC  9(002)     VALUE 30.
           05  K-31                 PIC  9(002)     VALUE 31.
           05  K-32                 PIC  9(002)     VALUE 32.
           05  K-33                 PIC  9(002)     VALUE 33.
           05  K-34                 PIC  9(002)     VALUE 34.
           05  K-35                 PIC  9(002)     VALUE 35.
           05  K-36                 PIC  9(002)     VALUE 36.
           05  K-37                 PIC  9(002)     VALUE 37.
           05  K-38                 PIC  9(002)     VALUE 38.
           05  K-39                 PIC  9(002)     VALUE 39.
           05  K-40                 PIC  9(002)     VALUE 40.
           05  K-41                 PIC  9(002)     VALUE 41.
           05  K-42                 PIC  9(002)     VALUE 42.
           05  K-43                 PIC  9(002)     VALUE 43.
           05  K-44                 PIC  9(002)     VALUE 44.
           05  K-45                 PIC  9(002)     VALUE 45.
           05  K-46                 PIC  9(002)     VALUE 46.
           05  K-47                 PIC  9(002)     VALUE 47.
           05  K-48                 PIC  9(002)     VALUE 48.
           05  K-49                 PIC  9(002)     VALUE 49.
           05  K-50                 PIC  9(002)     VALUE 50.
           05  K-51                 PIC  9(002)     VALUE 51.
           05  K-52                 PIC  9(002)     VALUE 52.
           05  K-53                 PIC  9(002)     VALUE 53.
           05  K-54                 PIC  9(002)     VALUE 54.
           05  K-55                 PIC  9(002)     VALUE 55.
           05  K-56                 PIC  9(002)     VALUE 56.
           05  K-57                 PIC  9(002)     VALUE 57.
           05  K-58                 PIC  9(002)     VALUE 58.
           05  K-59                 PIC  9(002)     VALUE 59.
           05  K-60                 PIC  9(002)     VALUE 60.
           05  K-61                 PIC  9(002)     VALUE 61.
           05  K-62                 PIC  9(002)     VALUE 62.
           05  K-63                 PIC  9(002)     VALUE 63.
           05  K-64                 PIC  9(002)     VALUE 64.
           05  K-65                 PIC  9(002)     VALUE 65.
           05  K-66                 PIC  9(002)     VALUE 66.
           05  K-67                 PIC  9(002)     VALUE 67.
           05  K-68                 PIC  9(002)     VALUE 68.
           05  K-69                 PIC  9(002)     VALUE 69.
           05  K-70                 PIC  9(002)     VALUE 70.
           05  K-71                 PIC  9(002)     VALUE 71.
           05  K-72                 PIC  9(002)     VALUE 72.
           05  K-73                 PIC  9(002)     VALUE 73.
           05  K-74                 PIC  9(002)     VALUE 74.
           05  K-75                 PIC  9(002)     VALUE 75.
           05  K-76                 PIC  9(002)     VALUE 76.
           05  K-77                 PIC  9(002)     VALUE 77.
           05  K-78                 PIC  9(002)     VALUE 78.
           05  K-79                 PIC  9(002)     VALUE 79.
           05  K-80                 PIC  9(002)     VALUE 80.
           05  K-81                 PIC  9(002)     VALUE 81.
           05  K-82                 PIC  9(002)     VALUE 82.
           05  K-83                 PIC  9(002)     VALUE 83.
           05  K-84                 PIC  9(002)     VALUE 84.
           05  K-85                 PIC  9(002)     VALUE 85.
           05  K-86                 PIC  9(002)     VALUE 86.
           05  K-87                 PIC  9(002)     VALUE 87.
           05  K-88                 PIC  9(002)     VALUE 88.
           05  K-89                 PIC  9(002)     VALUE 89.
           05  K-90                 PIC  9(002)     VALUE 90.
           05  K-91                 PIC  9(002)     VALUE 91.
           05  K-92                 PIC  9(002)     VALUE 92.
           05  K-93                 PIC  9(002)     VALUE 93.
           05  K-94                 PIC  9(002)     VALUE 94.
           05  K-95                 PIC  9(002)     VALUE 95.
           05  K-96                 PIC  9(002)     VALUE 96.
           05  K-97                 PIC  9(002)     VALUE 97.
           05  K-98                 PIC  9(002)     VALUE 98.
           05  K-99                 PIC  9(002)     VALUE 99.
           05  K-100                PIC  9(003)     VALUE 100.
           05  K-101                PIC  9(003)     VALUE 101.
           05  K-102                PIC  9(003)     VALUE 102.
           05  K-103                PIC  9(003)     VALUE 103.
           05  K-104                PIC  9(003)     VALUE 104.
           05  K-105                PIC  9(003)     VALUE 105.
           05  K-106                PIC  9(003)     VALUE 106.
           05  K-107                PIC  9(003)     VALUE 107.
           05  K-108                PIC  9(003)     VALUE 108.
           05  K-109                PIC  9(003)     VALUE 109.
           05  K-110                PIC  9(003)     VALUE 110.
           05  K-111                PIC  9(003)     VALUE 111.
           05  K-112                PIC  9(003)     VALUE 112.
           05  K-113                PIC  9(003)     VALUE 113.
           05  K-114                PIC  9(003)     VALUE 114.
           05  K-115                PIC  9(003)     VALUE 115.
           05  K-116                PIC  9(003)     VALUE 116.
           05  K-117                PIC  9(003)     VALUE 117.
           05  K-118                PIC  9(003)     VALUE 118.
           05  K-119                PIC  9(003)     VALUE 119.
           05  K-120                PIC  9(003)     VALUE 120.
           05  K-121                PIC  9(003)     VALUE 121.
           05  K-122                PIC  9(003)     VALUE 122.
           05  K-123                PIC  9(003)     VALUE 123.
           05  K-124                PIC  9(003)     VALUE 124.
           05  K-125                PIC  9(003)     VALUE 125.
           05  K-126                PIC  9(003)     VALUE 126.
           05  K-127                PIC  9(003)     VALUE 127.
           05  K-128                PIC  9(003)     VALUE 128.
           05  K-129                PIC  9(003)     VALUE 129.
           05  K-130                PIC  9(003)     VALUE 130.
           05  K-131                PIC  9(003)     VALUE 131.
           05  K-132                PIC  9(003)     VALUE 132.
           05  K-133                PIC  9(003)     VALUE 133.
           05  K-134                PIC  9(003)     VALUE 134.
           05  K-135                PIC  9(003)     VALUE 135.
           05  K-136                PIC  9(003)     VALUE 136.
           05  K-137                PIC  9(003)     VALUE 137.
           05  K-138                PIC  9(003)     VALUE 138.
           05  K-139                PIC  9(003)     VALUE 139.
           05  K-140                PIC  9(003)     VALUE 140.
           05  K-141                PIC  9(003)     VALUE 141.
           05  K-142                PIC  9(003)     VALUE 142.
           05  K-143                PIC  9(003)     VALUE 143.
           05  K-144                PIC  9(003)     VALUE 144.
           05  K-145                PIC  9(003)     VALUE 145.
           05  K-146                PIC  9(003)     VALUE 146.
           05  K-147                PIC  9(003)     VALUE 147.
           05  K-148                PIC  9(003)     VALUE 148.
           05  K-149                PIC  9(003)     VALUE 149.
           05  K-150                PIC  9(003)     VALUE 150.
           05  K-151                PIC  9(003)     VALUE 151.
           05  K-152                PIC  9(003)     VALUE 152.
           05  K-153                PIC  9(003)     VALUE 153.
           05  K-154                PIC  9(003)     VALUE 154.
           05  K-155                PIC  9(003)     VALUE 155.
           05  K-156                PIC  9(003)     VALUE 156.
           05  K-157                PIC  9(003)     VALUE 157.
           05  K-158                PIC  9(003)     VALUE 158.
           05  K-159                PIC  9(003)     VALUE 159.
           05  K-160                PIC  9(003)     VALUE 160.
           05  K-161                PIC  9(003)     VALUE 161.
           05  K-162                PIC  9(003)     VALUE 162.
           05  K-163                PIC  9(003)     VALUE 163.
           05  K-164                PIC  9(003)     VALUE 164.
           05  K-165                PIC  9(003)     VALUE 165.
           05  K-166                PIC  9(003)     VALUE 166.
           05  K-167                PIC  9(003)     VALUE 167.
           05  K-168                PIC  9(003)     VALUE 168.
           05  K-169                PIC  9(003)     VALUE 169.
           05  K-170                PIC  9(003)     VALUE 170.
           05  K-171                PIC  9(003)     VALUE 171.
           05  K-172                PIC  9(003)     VALUE 172.
           05  K-173                PIC  9(003)     VALUE 173.
           05  K-174                PIC  9(003)     VALUE 174.
           05  K-175                PIC  9(003)     VALUE 175.
           05  K-176                PIC  9(003)     VALUE 176.
           05  K-177                PIC  9(003)     VALUE 177.
           05  K-178                PIC  9(003)     VALUE 178.
           05  K-179                PIC  9(003)     VALUE 179.
           05  K-180                PIC  9(003)     VALUE 180.
           05  K-181                PIC  9(003)     VALUE 181.
           05  K-182                PIC  9(003)     VALUE 182.
           05  K-183                PIC  9(003)     VALUE 183.
           05  K-184                PIC  9(003)     VALUE 184.
           05  K-185                PIC  9(003)     VALUE 185.
           05  K-186                PIC  9(003)     VALUE 186.
           05  K-187                PIC  9(003)     VALUE 187.
           05  K-188                PIC  9(003)     VALUE 188.
           05  K-189                PIC  9(003)     VALUE 189.
           05  K-190                PIC  9(003)     VALUE 190.
           05  K-191                PIC  9(003)     VALUE 191.
           05  K-192                PIC  9(003)     VALUE 192.
           05  K-193                PIC  9(003)     VALUE 193.
           05  K-194                PIC  9(003)     VALUE 194.
           05  K-195                PIC  9(003)     VALUE 195.
           05  K-196                PIC  9(003)     VALUE 196.
           05  K-197                PIC  9(003)     VALUE 197.
           05  K-198                PIC  9(003)     VALUE 198.
           05  K-199                PIC  9(003)     VALUE 199.
           05  K-200                PIC  9(003)     VALUE 200.
      * ***************************************************************
      *   BEI K-200 IST SCHLUSS!! ALLE DARÜBERHINAUS BENÖTIGTEN       *
      *   ZAHLEN-KONSTANTEN WERDEN IN DEN JEWEILIGEN PROGRAMMEN       *
      *   DEKLARIERT.                                                 *
      * ***************************************************************

           05  K-500                PIC  9(003)     VALUE 500.
      *
      *   ALPHANUMERISCHE KONSTANTEN
      *    VALUE-TYPEN
      *
       01  FILLER.
           05  K-PROZENT            PIC  X(001)     VALUE '%'.
           05  K-PARAGRAPH          PIC  X(001)     VALUE '§'.
           05  K-STERN              PIC  X(001)     VALUE '*'.
           05  K-A                  PIC  X(001)     VALUE 'A'.
           05  K-B                  PIC  X(001)     VALUE 'B'.
           05  K-C                  PIC  X(001)     VALUE 'C'.
           05  K-D                  PIC  X(001)     VALUE 'D'.
           05  K-E                  PIC  X(001)     VALUE 'E'.
           05  K-F                  PIC  X(001)     VALUE 'F'.
           05  K-G                  PIC  X(001)     VALUE 'G'.
           05  K-H                  PIC  X(001)     VALUE 'H'.
           05  K-I                  PIC  X(001)     VALUE 'I'.
           05  K-K                  PIC  X(001)     VALUE 'K'.
           05  K-M                  PIC  X(001)     VALUE 'M'.
           05  K-P                  PIC  X(001)     VALUE 'P'.
           05  K-Q                  PIC  X(001)     VALUE 'Q'.
           05  K-R                  PIC  X(001)     VALUE 'R'.
           05  K-S                  PIC  X(001)     VALUE 'S'.
           05  K-T                  PIC  X(001)     VALUE 'T'.
           05  K-U                  PIC  X(001)     VALUE 'U'.
           05  K-V                  PIC  X(001)     VALUE 'V'.
           05  K-W                  PIC  X(001)     VALUE 'W'.
           05  K-X                  PIC  X(001)     VALUE 'X'.
           05  K-Z                  PIC  X(001)     VALUE 'Z'.
           05  K-L                  PIC  X(001)     VALUE 'L'.
           05  K-DM                 PIC  X(003)     VALUE 'DM '.
           05  K-OES                PIC  X(003)     VALUE 'OES'.
           05  K-OES-2              PIC  X(003)     VALUE 'ÖS '.
      **** WAEHRUNGEN GEMAESS ISO-CODE  -  ANFANG
           05  K-DEM                PIC  X(003)     VALUE 'DEM'.
           05  K-EUR                PIC  X(003)     VALUE 'EUR'.
           05  K-CAD                PIC  X(003)     VALUE 'CAD'.
           05  K-CHF                PIC  X(003)     VALUE 'CHF'.
           05  K-GBP                PIC  X(003)     VALUE 'GBP'.
           05  K-USD                PIC  X(003)     VALUE 'USD'.
           05  K-ATS                PIC  X(003)     VALUE 'ATS'.
      **** WAEHRUNGEN GEMAESS ISO-CODE  -  ENDE
           05  K-JA                 PIC  X(001)     VALUE 'J'.
           05  K-NEIN               PIC  X(001)     VALUE 'N'.
           05  K-PUNKT              PIC  X(001)     VALUE '.'.
           05  K-DOPPELPUNKT        PIC  X(001)     VALUE ':'.
           05  K-KOMMA              PIC  X(001)     VALUE ','.
           05  K-STRICH             PIC  X(001)     VALUE '-'.
           05  K-QUERSTRICH         PIC  X(001)     VALUE '/'.
           05  K-UNTERSTRICH        PIC  X(001)     VALUE '_'.
           05  K-PLUS               PIC  X(001)     VALUE '+'.
           05  K-EOF                PIC  X(003)     VALUE 'EOF'.
           05  K-1NK                PIC  999V99     VALUE 1,00.
           05  K-10NK               PIC  999V99     VALUE 10,00.
           05  K-50NK               PIC  999V99     VALUE 50,00.
           05  K-100NK              PIC  999V99     VALUE 100,00.
           05  K-MINUS-1            PIC S9(001)     VALUE -1.
      *
      *    ABSTRAKTE KONSTANTEN-TYPEN
      *
       01  FILLER.
           05  K-SCHLUEHILFE        PIC  X(001)     VALUE '?'.
           05  K-HILFE              PIC  X(001)     VALUE '?'.
           05  K-AUSWAHL            PIC  X(001)     VALUE 'X'.
           05  K-UPD                PIC  X(001)     VALUE 'U'.
           05  K-INS                PIC  X(001)     VALUE 'I'.
           05  K-DEL                PIC  X(001)     VALUE 'D'.
           05  K-AEND               PIC  X(001)     VALUE 'A'.
           05  K-LOE                PIC  X(001)     VALUE 'L'.
           05  K-EINF               PIC  X(001)     VALUE 'E'.
           05  K-DETAIL             PIC  X(001)     VALUE 'D'.
           05  K-PRIVAT             PIC  X(001)     VALUE 'P'.
           05  K-BUERO              PIC  X(001)     VALUE 'B'.
           05  K-TAUSCH             PIC  X(001)     VALUE 'T'.
           05  K-KORR               PIC  X(001)     VALUE 'K'.
           05  K-COPY               PIC  X(001)     VALUE 'C'.
           05  K-VORBELEGT          PIC  X(001)     VALUE 'V'.
           05  K-BELEGT             PIC  X(001)     VALUE 'N'.
           05  K-DAT-VON-MIN        PIC  X(026)     VALUE
                '1900-01-01-00.00.00.000000'.
           05  K-DAT-BIS-MAX-ISO    PIC  X(010)     VALUE
                '9999-12-31'.
           05  K-DAT-BIS-MAX-EURO   PIC  X(010)     VALUE
                '31.12.9999'.
           05  K-DAT-FALSCH-EURO    PIC  X(010)     VALUE
                '11.11.1111'.
           05  K-DAT-MIN-EURO       PIC X(010)      VALUE
                '01.01.1900'.
           05  K-AUN-MASTF          PIC  X(001)     VALUE 'A'.
           05  K-AUN-REPR           PIC  X(001)     VALUE 'A'.
           05  K-ANZ-HIST           PIC  X(001)     VALUE 'H'.
           05  K-VM-INAKTIV         PIC  X(002)     VALUE ',I'.
           05  K-STORNO             PIC  X(003)     VALUE 'STO'.
           05  K-WIEDERINKRAFT      PIC  X(003)     VALUE 'WIK'.
           05  K-REKLAMATION        PIC  X(003)     VALUE 'REK'.
           05  K-NPM                PIC  X(003)     VALUE 'NPM'.
           05  K-VTG                PIC  X(003)     VALUE 'VTG'.
           05  K-BTG                PIC  X(003)     VALUE 'BTG'.
           05  K-DAT                PIC  X(003)     VALUE 'DAT'.
           05  K-ABGERECHNET        PIC  X(03)      VALUE 'ABG'.
           05  K-ANZEIGE-DGV        PIC  X(03)      VALUE 'AND'.
           05  K-ANZEIGE-VSTAT      PIC  X(03)      VALUE 'ANV'.
           05  K-SICHT-VERTRAG      PIC  X(03)      VALUE 'VER'.
           05  K-FCS-HIS            PIC  X(03)      VALUE 'HIS'.
           05  K-FCS-EVS            PIC  X(03)      VALUE 'EVS'.
           05  K-FCS-VOR            PIC  X(03)      VALUE 'VOR'.
           05  K-FCS-NOTIZ          PIC  X(03)      VALUE 'NTZ'.
           05  K-FCS-UBV            PIC  X(03)      VALUE 'UBV'.
           05  K-FCS-FEK            PIC  X(03)      VALUE 'FEK'.
           05  K-FCS-PVW            PIC  X(03)      VALUE 'PVW'.
           05  K-FCS-PES            PIC  X(03)      VALUE 'PES'.
           05  K-FCS-SKF            PIC  X(03)      VALUE 'SKF'.
           05  K-FCS-NEU            PIC  X(03)      VALUE 'NEU'.
           05  K-FCS-AEN            PIC  X(03)      VALUE 'AEN'.
           05  K-FCS-LOE            PIC  X(03)      VALUE 'LOE'.
           05  K-FCS-ANZ            PIC  X(03)      VALUE 'ANZ'.
           05  K-EVS                PIC  X(12)
                                           VALUE 'EVS-GESCHÄFT'.
           05  K-UNDEF              PIC  X(08)
                                           VALUE 'UNDEF.  '.
      ***  DIE FOLGENDEN KONSTANTEN WERDEN BENöTIGT UM
      ***  STANDARD-FELD-BEZEICHNUNGEN AUF DER MAP AUSZUGEBEN.
           05  K-FELD-BEZ-FC        PIC  X(002)   VALUE 'FC'.
           05  K-FELD-BEZ-FC-S      PIC  X(005)   VALUE 'FC-S:'.
           05  K-FELD-BEZ-SEITE     PIC  X(006)   VALUE 'Seite:'.
           05  K-FELD-BEZ-PRODUKT   PIC  X(008)   VALUE 'Produkt:'.
      *
      *   SQLCODES
       01  FILLER.
           05  KS-OK                          PIC S9(04) VALUE 0.
           05  KS-NOT-FOUND                   PIC S9(04) VALUE 100.
           05  KS-DAT-SYNTAX-INVALID          PIC S9(04) VALUE -180.
           05  KS-DAT-INVALID                 PIC S9(04) VALUE -181.
           05  KS-CURSOR-NOT-OPEN             PIC S9(04) VALUE -501.
           05  KS-DUPLICATE-VALUE             PIC S9(04) VALUE -803.
           05  KS-MULTIPLE-ROWS               PIC S9(04) VALUE -811.
           05  KS-UNAVAILABLE-RESOURCE        PIC S9(04) VALUE -904.
           05  KS-TIMEOUT-WITH-ROLLBACK       PIC S9(04) VALUE -911.
           05  KS-TIMEOUT-NO-ROLLBACK         PIC S9(04) VALUE -913.
      *
      *   FILE-STATUS
       01  FILLER.
           05  KF-OK                          PIC  X(02) VALUE '00'.
           05  KF-OK-FALSCHE-SATZLAENGE       PIC  X(02) VALUE '04'.
           05  KF-EOF                         PIC  X(02) VALUE '10'.
           05  KF-DUPLICATE-RECORD            PIC  X(02) VALUE '22'.
           05  KF-NOT-FOUND                   PIC  X(02) VALUE '23'.
           05  KF-FALSCHER-OPEN-MODUS         PIC  X(02) VALUE '37'.
      *
      *
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRAT0009) ********
