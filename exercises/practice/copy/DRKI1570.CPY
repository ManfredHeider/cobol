      *****************************************************************
      *    COPY-MEMBER: DRKI1570                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1570                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  06.08.2003/14:59:39
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1570
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI1570.
           05  PI-DRKI1570-PROGRAMM             PIC  X(008).
           05  PI-DRKI1570-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI1570-DATEN.
             10  PI-DRKI1570-KEY-VON            PIC  X(013).
             10  PI-DRKI1570-KEY-BIS            PIC  X(013).
             10  PI-DRKI1570-LFDNR-VON          PIC  S9(04) COMP.
             10  PI-DRKI1570-LFDNR-BIS          PIC  S9(04) COMP.

      *
       01  PO-DRKI1570.
           05  PO-DRKI1570-RC                 PIC  X(003).
               88 PO-DRKI1570-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1570-RC-EOF                       VALUE 'EOF'.
               88 PO-DRKI1570-RC-LER                       VALUE 'LER'.
      *
           05  PO-DRKI1570-DATEN.

             10 PO-DRKI1570-VERTRAGSID          PIC X(13).
             10 PO-DRKI1570-KDULAUFENDENUMMER   PIC  S9(04) COMP.
             10 PO-DRKI1570-VBALTID             PIC X(13).
             10 PO-DRKI1570-VBALTROLLENKZ       PIC X(1).
             10 PO-DRKI1570-VBNEUID             PIC X(13).
             10 PO-DRKI1570-VBNEUROLLENKZ       PIC X(1).


      *
      ******* ENDE COPY-MEMBER **************************************
