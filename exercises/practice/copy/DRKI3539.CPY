      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3539.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3539.CPv  $
      *
      *   Rev 2.2   Feb 18 2020 13:51:18   t13ex27
      *FKI01626: Korrektur "KDA vs. Antragssystem"
      *
      *   Rev 2.1   Jan 07 2019 15:29:44   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.0   Dec 12 2018 13:19:39 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3539                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3539                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.12.2018 / 13:19:39 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3539
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3539.
           05  PI-DRKI3539-KEY.


ANF   *        %M_PI_KEY%
      *      10 PI-DRKI3539-KNDBDSGSPERRDAT   PIC  X(010).
      *      10 PI-DRKI3539-KNDMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI3539-KNDNUMMER   PIC S9(9) COMP.
END   *

      *
       01  PO-DRKI3539.
           05  PO-DRKI3539-RC                     PIC  X(003).
               88 PO-DRKI3539-RC-OK               VALUE 'OK '.
               88 PO-DRKI3539-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3539-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3539-KNDALTKUNDE     PIC  X(010).
             10 PO-DRKI3539-KNDANREDEID     PIC S9(4) COMP.
             10 PO-DRKI3539-KNDANTRAGSKNR   PIC S9(9) COMP.
             10 PO-DRKI3539-KNDANZBETREUER  PIC S9(4) COMP.
             10 PO-DRKI3539-KNDANZNAME      PIC  X(0030).
             10 PO-DRKI3539-KNDANZVORNAME   PIC  X(0030).
             10 PO-DRKI3539-KNDGEANDERTPGM  PIC  X(0008).
             10 PO-DRKI3539-KNDGEANDERTVON  PIC  X(0004).
             10 PO-DRKI3539-KNDGEBGRUENDDAT PIC  X(010).
             10 PO-DRKI3539-KNDGESCHLECHT   PIC  X(0001).
             10 PO-DRKI3539-KNDNEUKUNDE     PIC  X(010).
             10 PO-DRKI3539-KNDPERSONENID   PIC  X(0013).
             10 PO-DRKI3539-KNDSTATUSID     PIC  X(0001).
             10 PO-DRKI3539-KNDSUCHNAME     PIC  X(0030).
             10 PO-DRKI3539-KNDSUCHVORNAME  PIC  X(0030).
             10 PO-DRKI3539-KNDWIK          PIC  X(010).
             10 PO-DRKI3539-KNDMANDANTNR    PIC S9(9) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
