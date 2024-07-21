     *****************************************************************
      *    COPY-MEMBER: DRVB1185                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1185                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    10.01.2018                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00573!10.01.18!2.1 ! NEU                       ! Walb     *
120722* VLA-5045!08.07.22!2.2 ! Num. gepackte Felder      ! HEIM     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1185
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1185.
           10 PI-DRVB1185-MA-ST-NR            PIC  S9(009) COMP.
           10 PI-DRVB1185-DAT-VON             PIC  X(010).
           10 PI-DRVB1185-DAT-BIS             PIC  X(010).
           10 PI-DRVB1185-MS-NR-NEU           PIC  S9(004) COMP.
120722     10 PI-DRVB1185-MS-EH-EG-MAX        PIC  S9(010)V9 COMP-3.
      *
       01  PO-DRVB1185.
           05  PO-DRVB1185-RC                 PIC  X(003).
               88 PO-DRVB1185-RC-OK                VALUE 'OK '.
               88 PO-DRVB1185-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB1185-RC-NOT-FOUND         VALUE 'NFD'.
      *
120722     05  PO-DRVB1185-NET-EG-GESAMT      PIC  S9(010)V9 COMP-3.
120722     05  PO-DRVB1185-NET-EG-ABZUG       PIC  S9(010)V9 COMP-3.
120722     05  PO-DRVB1185-NET-GES-GESAMT     PIC  S9(010)V9 COMP-3.
120722     05  PO-DRVB1185-NET-GES-BEREINIGT  PIC  S9(010)V9 COMP-3.
      ******* ENDE COPY-MEMBER **************************************

