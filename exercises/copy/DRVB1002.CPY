      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1002                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0093                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    16.08.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00367!16.11.10! 2.1! NEU                       !GUTFLEISCH*
      * --------+--------+----+---------------------------+----------*
      * AVB01400!27.06.16! 2.2! Neues Feld                !Walb      *
      * --------+--------+----+---------------------------+----------*  
110123* VLA-5288!11.01.23! ANPASSEN DER UNTERSCHEIDUNG ZW.! THURMANN *
      *         !        ! LB-PRAXIS UND LB-ALLFINANZ     !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1002                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1002.
          05 PI-DRVB1002-UFKT-CODE          PIC   9(002).
          05 PI-DRVB1002-ANSPR-VOM          PIC   X(010).
          05 PI-DRVB1002-ANSPR-STATUS       PIC  S9(004) BINARY.
          05 PI-DRVB1002-LZUA-ST-NR         PIC  S9(009) BINARY.
110123    05 PI-DRVB1002-LZUA-OLZ-KNR       PIC   9(002).
      *
       01 PO-DRVB1002.
          05 PO-DRVB1002-RC                 PIC  X(003).
             88 PO-DRVB1002-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1002-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1002-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1002-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRVB1002-DATEN.
             10 PO-DRVB1002-LZUA-ANSPR-VOM     PIC   X(010).
             10 PO-DRVB1002-LZUA-ANSPR-BETRAG  PIC  S9(09)V9(02) COMP-3.
             10 PO-DRVB1002-KNR-WAEHRUNG       PIC   X(003).
             10 PO-DRVB1002-LZUA-ANSP-BET-UPFL PIC  S9(09)V9(02) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB1002 *****************************