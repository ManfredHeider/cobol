      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0019.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRRW0019.CPv  $
      *
      *   Rev 2.9   Oct 01 2019 17:52:08   t13ex71
      *FRW00090: Ausbilder-Bonus Diskontgutschriften 09/2019 - Copy
      *books und Upros sind auf M:ED nicht aktuell; neu aus- und wi
      *ecken (BPR)
      *
      *   Rev 2.8   Jun 27 2019 17:23:52   t13ex71
      *ASL00589: Verrechnung freiwillige Sonderprovisionen mit Soll
      *salden - UPRW0003/4/5 mit Copybooks (BPR)
      *
      *   Rev 2.7   Apr 16 2019 15:22:02   t13ex71
      *ASL00589: Verrechnung freiwillige Sonderprovisionen mit Soll
      *salden - UPRW0003/4/5 mit Copybooks (BPR)
      *
      *   Rev 2.6   Aug 05 2016 13:48:52   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003 (HWO)
      *
      *   Rev 2.5   Jul 27 2016 12:22:04   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003 (HWO)
      *
      *   Rev 2.4   Jul 22 2016 11:12:48   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003 (HWO)
      *
      *   Rev 2.3   Jul 14 2016 11:58:40   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003 (HWO)
      *
      *   Rev 2.2   Jun 14 2016 14:46:54   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003
      *
      *   Rev 2.1   May 31 2016 14:37:32   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Untern
      *ehmer UPRW0003
      *
      *   Rev 2.0   May 19 2016 11:49:42   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0019                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    19.05.2016                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!19.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
180119* ASL00589!18.01.19!2.2 ! Sollsaldo DVAG/ALLF1/ALLF2! B.Petsch  *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRRW0016 - Buchen FIBU-Schnittstelle UPRW0003     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRRW0019.
      ***  Daten
           05  PI-DRRW0019-DATEN.
               10  PI-DRRW0019-MA-ST-NR         PIC S9(009)    BINARY.
               10  PI-DRRW0019-PZ               PIC  X(001).
               10  PI-DRRW0019-DIR              PIC  9(003).
               10  PI-DRRW0019-STUFE            PIC  X(005).
               10  PI-DRRW0019-NAME             PIC  X(040).
               10  PI-DRRW0019-VORNAME          PIC  X(020).
               10  PI-DRRW0019-BETR-UPFL-NETTO  PIC S9(09)V99.
180119         10  PI-DRRW0019-BETR-DISKKTO     PIC S9(09)V99.
180119         10  PI-DRRW0019-SOLLSA-BES433    PIC  X(001).
180119             88 PI-DRRW0019-SOLLSA-BES433-J    VALUE 'J'.
180119             88 PI-DRRW0019-SOLLSA-BES433-N    VALUE 'N'.
               10  PI-DRRW0019-HINWEIS          PIC  X(025).
               10  PI-DRRW0019-TURNUS-JJJJMM    PIC  9(006).
               10  FILLER REDEFINES PI-DRRW0019-TURNUS-JJJJMM.
                   15 PI-DRRW0019-TURNUS-JJJJ.
                      20 PI-DRRW0019-TURNUS-JH  PIC  9(002).
                      20 PI-DRRW0019-TURNUS-JJ  PIC  9(002).
                   15 PI-DRRW0019-TURNUS-MM     PIC  9(002).
               10  PI-DRRW0019-GESELLSCHAFT     PIC  X(012).
               10  PI-DRRW0019-ANSPR-GRUND      PIC S9(004)    BINARY.
               10  PI-DRRW0019-ANSPR-STATUS     PIC  X(001).
                   88 PI-DRRW0019-ANSPR-AUSZAHLEN    VALUE 'A'.
                   88 PI-DRRW0019-ANSPR-NICHT-AUSZ   VALUE 'N'.
               10  PI-DRRW0019-BUCHTEXT         PIC  X(035).
               10  PI-DRRW0019-MANDANT          PIC  X(003).
               10  PI-DRRW0019-BELEGART         PIC  X(003).
               10  PI-DRRW0019-KOSTENSTELLE     PIC  X(004).
               10  PI-DRRW0019-KONTO-SOLL       PIC  X(012).
               10  PI-DRRW0019-KONTO-HABEN      PIC  X(012).
               10  PI-DRRW0019-LISTE            PIC  X(005).
               10  PI-DRRW0019-UST-SATZ         PIC S9(003)V99.
               10  PI-DRRW0019-STEUERJAHR       PIC  9(004).
               10  PI-DRRW0019-BEZUGSJAHR       PIC  9(004).
               10  PI-DRRW0019-BUCHTEXT-FIBU    PIC  X(080).
               10  PI-DRRW0019-BELEGDATUM       PIC  X(008).
               10  PI-DRRW0019-BUCH-VERMERK     PIC  X(002).
               10  PI-DRRW0019-KTY-VERARB-TYP   PIC S9(004)    BINARY.
               10  PI-DRRW0019-KNR-VERARB-TYP   PIC S9(004)    BINARY.
               10  PI-DRRW0019-ZIELSTATUS       PIC  X(001).
                   88 PI-DRRW0019-ZIELSTATUS-K       VALUE 'K'.
                   88 PI-DRRW0019-ZIELSTATUS-U       VALUE 'U'.
      *
       01  PO-DRRW0019.
         03  PO-DRRW0019-RC                     PIC  X(03).
               88  PO-DRRW0019-RC-OK                 VALUE 'OK '.
               88  PO-DRRW0019-RC-NOT-OK             VALUE 'NOK'.
      ****
      ******************** ENDE COPY DRRW0019 ************************
 
 
 
 
 
 
 


