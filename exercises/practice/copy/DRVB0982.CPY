      *****************************************************************
      *    COPY-MEMBER: DRVB0982                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0982                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Boll                                             *
      *    DATUM:    20.07.2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00352!20.07.10! 01 ! NEU                       ! U. Boll  *
200922* VLA-5363!20.09.22!    ! Num. gepackte Felder      ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0982
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0982.
           05  PI-DRVB0982-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0982-DATUM-VON          PIC  X(010).
           05  PI-DRVB0982-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0982.
           05  PO-DRVB0982-RC                 PIC  X(003).
               88 PO-DRVB0982-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0982-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0982-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0982-RC-ERR                       VALUE 'ERR'.
      *
200922     05  PO-DRVB0982-EH-GS-GR           PIC  S9(010)V9 COMP-3.
200922     05  PO-DRVB0982-EH-GS-EG           PIC  S9(010)V9 COMP-3.
200922     05  PO-DRVB0982-EH-GS-GS           PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


