      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3111.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3111.CPv  $
      *
      *   Rev 2.5   Feb 18 2020 13:48:58   t13ex27
      *FKI01626: Korrektur "KDA vs. Antragssystem"
      *
      *   Rev 2.4   Jan 07 2019 15:28:40   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.3   Dec 19 2016 10:59:58   t13ex27
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.2   Nov 16 2016 08:48:56   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.1   Oct 27 2016 11:43:44   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.0   Oct 12 2016 14:32:50 T13EX73
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3111                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3111                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.12.2018 / 13:19:37 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI01626!26.08.19!2.5 ! Mandant und Sperrdatum    ! T13EX27  *
      *         !        !    ! aus PI entfernt           !          *
      *         !        !    ! Mandant in PO eingefügt   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3111
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3111.
           05  PI-DRKI3111-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3111-KNDANTRAGSKNR   PIC S9(9) COMP.
      *      10 PI-DRKI3111-KNDBDSGSPERRDAT   PIC  X(010).
      *      10 PI-DRKI3111-KNDMANDANTNR   PIC S9(9) COMP.
END   *

      *
       01  PO-DRKI3111.
           05  PO-DRKI3111-RC                     PIC  X(003).
               88 PO-DRKI3111-RC-OK               VALUE 'OK '.
               88 PO-DRKI3111-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3111-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3111-KNDALTKUNDE     PIC  X(010).
             10 PO-DRKI3111-KNDANREDEID     PIC S9(4) COMP.
             10 PO-DRKI3111-KNDANZBETREUER  PIC S9(4) COMP.
             10 PO-DRKI3111-KNDANZNAME      PIC  X(0030).
             10 PO-DRKI3111-KNDANZVORNAME   PIC  X(0030).
             10 PO-DRKI3111-KNDGEANDERTPGM  PIC  X(0008).
             10 PO-DRKI3111-KNDGEANDERTVON  PIC  X(0004).
             10 PO-DRKI3111-KNDGEBGRUENDDAT PIC  X(010).
             10 PO-DRKI3111-KNDGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3111-KNDNEUKUNDE     PIC  X(010).
             10 PO-DRKI3111-KNDNUMMER       PIC S9(9) COMP.
             10 PO-DRKI3111-KNDPERSONENID   PIC  X(0013).
             10 PO-DRKI3111-KNDSTATUSID     PIC  X(0001).
             10 PO-DRKI3111-KNDSUCHNAME     PIC  X(0030).
             10 PO-DRKI3111-KNDSUCHVORNAME  PIC  X(0030).
             10 PO-DRKI3111-KNDWIK          PIC  X(010).
             10 PO-DRKI3111-KNDMANDANTNR    PIC S9(9) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


