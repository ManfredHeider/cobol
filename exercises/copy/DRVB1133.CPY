      *****************************************************************
      *    COPY-MEMBER: DRVB1133                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1133                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDI WÖLFLIK                                    *
      *    DATUM:    21.04.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!21.04.16!    ! NEU                       ! H.Wölflik*
200922* VLA-5363!20.09.22!    ! Num. gepackte Felder      ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1133
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1133.
           05  PI-DRVB1133-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1133-DATUM-VON          PIC  X(010).
           05  PI-DRVB1133-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1133.
           05  PO-DRVB1133-RC                 PIC  X(003).
               88 PO-DRVB1133-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1133-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1133-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1133-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1133-EH-GS-GR-EG         PIC  S9(010)V9 COMP-3.
           05  PO-DRVB1133-NI-EH-GS-GR-EG      PIC  S9(004)   COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
 

