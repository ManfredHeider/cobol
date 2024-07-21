      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1064 FUNKTION UMBUCHUNG EINER BUCHUNG    *
      *                          NACH EINER PE-STRUKTUR-äNDERUNG     *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    10.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * --------------------------------------------     ------------*
      * APA00012!10.09.98!NEU                             !BOHLINGER *
      * APA02265!06.06.16!DGV 4-STELLIG                   !BUCH      *
      * APA02492!01.11.17!DGV 5-STELLIG                   !STORCH    *
      * APA02548!05.03.19!PI-BEREICH ERWEITERT            !STORCH    *
      * APA02796!15.07.19!PI-BEREICH ERWEITERT            !STORCH    *
      * DSY04437!06.07.22!PI-BEREICH ERWEITERT            !STORCH    *  
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1064                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1064.
             10 PI-DRPA1064-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1064-PDPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1064-PEST-ID-BN         PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1064-S-PEST-ID-BN       PIC  X(001).
060613       10 PI-DRPA1064-DGV-BN             PIC  X(005).
             10 PI-DRPA1064-S-DGV-BN           PIC  X(001).
050319*      10 PI-DRPA1064-S-NACHFOLGE        PIC  X(001).
150719       10 PI-DRPA1064-S-PRO-RATA         PIC  X(001).
060722       10 PI-DRPA1064-S-RFT              PIC  X(001).             
 
       01 PO-DRPA1064.
          05 PO-DRPA1064-RC                    PIC  X(003).
             88 PO-DRPA1064-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1064-RC-UMB-UNZULAESSIG          VALUE 'UUZ'.
 
          05 PO-DRPA1064-MELDUNG.
             10 PO-DRPA1064-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1064-MELDUNG-NR         PIC S9(004) BINARY.
 
          05 PO-DRPA1064-VSTATUS               PIC  X(004).
 
          05 PO-DRPA1064-MAP-STEUERUNG.
             10 PO-DRPA1064-MAP-ST-DGV.
                20 FELD-INTENSIV              PIC X(001).
                20 FELD-OFFEN                 PIC X(001).
                20 FELD-CURSOR-KZ             PIC X(001).
      *
      ******** Ende Copy-Member **************************************
