      *****************************************************************
      *    COPY-MEMBER: DRVB1132                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1132                    *
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
      *    FÜR DIE FUNKTION DRVB1132
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1132.
           05  PI-DRVB1132-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1132-DATUM-VON          PIC  X(010).
           05  PI-DRVB1132-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1132.
           05  PO-DRVB1132-RC                 PIC  X(003).
               88 PO-DRVB1132-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1132-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1132-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1132-RC-ERR                       VALUE 'ERR'.
      *
200922     05  PO-DRVB1132-EH-GS-GR           PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

