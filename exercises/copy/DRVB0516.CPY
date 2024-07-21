      *****************************************************************
      *    COPY-MEMBER: DRVB0516                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0516                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    08.02.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00068!08.02.01! 01 ! NEU                       ! Walb     *
200922* VLA-5363!20.09.22!    ! Num. gepackte Felder      ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0516
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0516.
           05  PI-DRVB0516-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0516-DATUM-VON          PIC  X(010).
           05  PI-DRVB0516-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0516.
           05  PO-DRVB0516-RC                 PIC  X(003).
               88 PO-DRVB0516-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0516-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0516-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0516-RC-ERR                       VALUE 'ERR'.
      *
200922     05  PO-DRVB0516-EH-GS-GR           PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


