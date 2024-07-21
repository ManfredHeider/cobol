      *****************************************************************
      *    COPY-MEMBER: DRVB0606                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0606                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    19.10.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00333!19.10.02!1.0 ! NEU                       ! P.Duerr  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0435
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0606.
           05  PI-DRVB0606-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0606-DATUM-VON          PIC  X(010).
           05  PI-DRVB0606-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0606.
           05  PO-DRVB0606-RC                 PIC  X(003).
               88 PO-DRVB0606-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0606-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0606-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0606-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0606-EH-GS-GR-NETTO      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0606-NI-EH-GS-GR-NETTO   PIC  S9(004)   COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
