      *****************************************************************
      *    COPY-MEMBER: DRVB1131                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1131                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDI WÖLFLIK                                    *
      *    DATUM:    21.04.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!21.04.16! 01 ! NEU                       ! H.WÖLFLIK*
200922* VLA-5363!20.09.22! 02 ! Num. gepackte Felder      ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1131
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1131.
           05  PI-DRVB1131-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1131-DATUM-VON          PIC  X(010).
           05  PI-DRVB1131-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1131.
           05  PO-DRVB1131-RC                 PIC  X(003).
               88 PO-DRVB1131-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1131-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1131-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1131-RC-ERR                       VALUE 'ERR'.
      *
200922     05  PO-DRVB1131-EH-GS-GR           PIC  S9(010)V9 COMP-3.
200922     05  PO-DRVB1131-EH-GS-EG           PIC  S9(010)V9 COMP-3.
200922     05  PO-DRVB1131-EH-GS-GS           PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

