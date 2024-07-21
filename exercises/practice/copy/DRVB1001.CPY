      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1001                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0093                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *                                                               *
      *    GESAMTES TUPEL AUS LZUA (OLZ TYP 2 UND 15)                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: R.GUTFLEISCH                                     *
      *    DATUM:    15.11.2010                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00367!15.11.10! 2.1! NEU                       !GUTFLEISCH*
      * --------+--------+----+---------------------------+----------*
110123* VLA-5288!11.01.23! ANPASSEN DER UNTERSCHEIDUNG ZW.! THURMANN *
      *         !        ! LB-PRAXIS UND LB-ALLFINANZ     !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1001                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB1001.
          05 PI-DRVB1001-LZUA-ST-NR         PIC  S9(009) BINARY.
          05 PI-DRVB1001-LZUA-ANSPR-VOM     PIC   X(010).
          05 PI-DRVB1001-KNR-ANSPR-GRUND    PIC  S9(004) BINARY.
110123    05 PI-DRVB1001-LZUA-OLZ-KNR       PIC  S9(002).
      *
       01 PO-DRVB1001.

          05 PO-DRVB1001-RC                 PIC  X(003).
             88 PO-DRVB1001-RC-OK                VALUE 'OK '.
             88 PO-DRVB1001-RC-LEER              VALUE 'LER'.
             88 PO-DRVB1001-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB1001-RC-NOT-FOUND         VALUE 'NFD'.

          05 PO-DRVB1001-DATEN.
             10 PO-DRVB1001-LZUA-TUPEL-GESAMT PIC   X(086).
      *
      ***** ENDE COPY-MEMBER DRVB1001 *****************************