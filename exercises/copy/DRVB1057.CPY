      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1057                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0096                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    24.04.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS01008!25.04.12!2.1 ! NEU                       !WILLIG    *
      *         !        !    !                           !           *
      *                                                               *
      * VLA-1995!12.03.21! NEUE VERSION EBB WG. KS2021    ! T13AE30   *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB1057                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1057.
          05 PI-DRVB1057-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB1057-KNR-TYP-BEFBED     PIC S9(004) BINARY.         
          05 PI-DRVB1057-LAUF-DAT           PIC  X(007).
      *                                     FORMAT MM.JJJJ
      *
       01 PO-DRVB1057.
          05 PO-DRVB1057-RC                 PIC  X(003).
             88 PO-DRVB1057-RC-OK                VALUE 'OK '.
             88 PO-DRVB1057-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1057-DATEN.
             10 PO-DRVB1057-EBB-TUPEL-GESAMT PIC  X(170).
      *
      ***** ENDE COPY-MEMBER DRVB1057 *****************************
      *
