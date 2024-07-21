      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2098.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2098.CPv  $
      *
      *   Rev 2.7   Nov 28 2018 11:51:10   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.6   Mar 23 2015 09:34:26   t13ex73
      *AKI01354: Kundenaktualisierung DBO-71 - Minderung von falsch
      *en Vertragszuordnungen
      *
      *   Rev 2.5   Sep 18 2012 13:57:20   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.4   Sep 30 2009 14:50:58   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.3   19 Nov 2007 09:55:24   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundenaktualisierung
      *
      *   Rev 2.2   18 Nov 2007 15:21:12   t13ex27
      *No change.
      *
      *   Rev 2.1   16 Oct 2007 14:25:32   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundensuche
      *
      *   Rev 2.0   24 Sep 2007 10:19:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2098                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2098                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00664!24.09.07!2.1 ! neu                       !I. Frenzel*
      * -------------------------------------------------------------*
      * AKI00852!31.08.09! 02 ! SUCHNAME AUF 30 STELLEN   ! HRO      *
      * AKI00886!28.11.11!2.5 ! Neue Kundenaktualisierung !I. Frenzel*
      * -------------------------------------------------------------*
050315* AKI01354!05.03.15! 02 ! AnredeID hinzufuegen      !D. Dylong *
      * AKI01639!24.10.18!2.7 ! Verbesserung Kundensuche  ! IFR      *
      *         !        !    ! DBO-2130                  !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2098
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI2098.
          05 PI-DRKI2098-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2098-UFKT-CODE                PIC  9(002).
          05 PI-DRKI2098-DATEN.
            07 PI-DRKI2098-SUCHNAME               PIC X(030).
            07 PI-DRKI2098-SUCHVORNAME            PIC X(030).
            07 PI-DRKI2098-GEBGRUENDDAT           PIC X(010).
            07 PI-DRKI2098-MANDANTNR              PIC S9(009) COMP.
      *
       01 PO-DRKI2098.
 
          05 PO-DRKI2098-RC                       PIC  X(003).
             88 PO-DRKI2098-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2098-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2098-RC-EOF                       VALUE 'EOF'.
 
          05 PO-DRKI2098-DATEN.
            10 PO-DRKI2098-KNDPERSONENID         PIC X(013).
            10 PO-DRKI2098-KNDGEBGRUENDDAT       PIC X(010).
 
            10 PO-DRKI2098-KNDANZBETREUER        PIC S9(4) COMP.
            10 PO-DRKI2098-KNDNUMMER             PIC S9(9) COMP.
            10 PO-DRKI2098-KNDSTATUSID           PIC X(01).
050315      10 PO-DRKI2098-KNDANREDEID           PIC S9(004) COMP.
            10 PO-DRKI2098-KNDSUCHVORNAME        PIC X(30).
            10 PO-DRKI2098-KNDANZVORNAME         PIC X(30).
      *
      ********** Ende Copy-Member DRKI2098 ****************************


