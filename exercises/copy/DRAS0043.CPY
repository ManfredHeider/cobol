      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.02.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0043.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAS0043.CPv  $
      *
      *   Rev 2.2   Mar 16 2016 13:25:50   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.1   Mar 16 2016 12:48:52   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.0   Feb 01 2016 13:36:26   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0043                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0043                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    01.02.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01158!01.02.16!    ! NEU                       ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAS0043
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0043.
           05  PI-DRAS0043-VB-NR              PIC  S9(009) COMP.
           05  PI-DRAS0043-DATUM-VON          PIC  X(10).
           05  PI-DRAS0043-DATUM-BIS          PIC  X(10).
      *
       01  PO-DRAS0043.
           05  PO-DRAS0043-RC                 PIC  X(003).
               88 PO-DRAS0043-RC-OK                        VALUE 'OK '.
               88 PO-DRAS0043-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAS0043-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRAS0043-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRAS0043-P-STUFE            PIC  S9(004) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************


