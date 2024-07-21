      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0021.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRRW0021.CPv  $
      *
      *   Rev 2.8   Oct 01 2019 17:52:40   t13ex71
      *FRW00090: Ausbilder-Bonus Diskontgutschriften 09/2019 - Copy
      *books und Upros sind auf M:ED nicht aktuell; neu aus- und wi
      *ecken (BPR)
      *
      *   Rev 2.7   Jun 27 2019 17:24:02   t13ex71
      *ASL00589: Verrechnung freiwillige Sonderprovisionen mit Soll
      *salden - UPRW0003/4/5 mit Copybooks (BPR)
      *
      *   Rev 2.6   Apr 16 2019 15:22:10   t13ex71
      *ASL00589: Verrechnung freiwillige Sonderprovisionen mit Soll
      *salden - UPRW0003/4/5 mit Copybooks (BPR)
      *
      *   Rev 2.5   Jul 27 2016 12:22:58   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004 (HWO)
      *
      *   Rev 2.4   Jul 22 2016 11:13:40   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004 (HWO)
      *
      *   Rev 2.3   Jul 14 2016 11:59:42   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004 (HWO)
      *
      *   Rev 2.2   Jun 14 2016 14:47:46   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004
      *
      *   Rev 2.1   May 31 2016 14:38:18   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Pfl. Klein-
      *Untern. UPRW0004
      *
      *   Rev 2.0   May 19 2016 11:52:58   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0021                                      *
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
      *    FUNKTION DRRW0016 - Buchen FIBU-Schnittstelle UPRW0004     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRRW0021.
      ***  Daten
           05  PI-DRRW0021-DATEN.
               10  PI-DRRW0021-MA-ST-NR         PIC S9(009)    BINARY.
               10  PI-DRRW0021-PZ               PIC  X(001).
               10  PI-DRRW0021-DIR              PIC  9(003).
               10  PI-DRRW0021-STUFE            PIC  X(005).
               10  PI-DRRW0021-NAME             PIC  X(040).
               10  PI-DRRW0021-VORNAME          PIC  X(020).
               10  PI-DRRW0021-BETR-UPFL        PIC S9(09)V99.
180119         10  PI-DRRW0021-BETR-DISKKTO     PIC S9(09)V99.
180119         10  PI-DRRW0021-SOLLSA-BES433    PIC  X(001).
180119             88 PI-DRRW0021-SOLLSA-BES433-J    VALUE 'J'.
180119             88 PI-DRRW0021-SOLLSA-BES433-N    VALUE 'N'.
               10  PI-DRRW0021-HINWEIS          PIC  X(025).
               10  PI-DRRW0021-TURNUS-JJJJMM    PIC  9(006).
               10  FILLER REDEFINES PI-DRRW0021-TURNUS-JJJJMM.
                   15 PI-DRRW0021-TURNUS-JJJJ.
                      20 PI-DRRW0021-TURNUS-JH  PIC  9(002).
                      20 PI-DRRW0021-TURNUS-JJ  PIC  9(002).
                   15 PI-DRRW0021-TURNUS-MM     PIC  9(002).
               10  PI-DRRW0021-GESELLSCHAFT     PIC  X(012).
               10  PI-DRRW0021-ANSPR-GRUND      PIC S9(004)    BINARY.
               10  PI-DRRW0021-ANSPR-STATUS     PIC  X(001).
                   88 PI-DRRW0021-ANSPR-AUSZAHLEN    VALUE 'A'.
                   88 PI-DRRW0021-ANSPR-NICHT-AUSZ   VALUE 'N'.
               10  PI-DRRW0021-BUCHTEXT         PIC  X(035).
               10  PI-DRRW0021-MANDANT          PIC  X(003).
               10  PI-DRRW0021-BELEGART         PIC  X(003).
               10  PI-DRRW0021-KOSTENSTELLE     PIC  X(004).
               10  PI-DRRW0021-KONTO-SOLL       PIC  X(012).
               10  PI-DRRW0021-KONTO-HABEN      PIC  X(012).
               10  PI-DRRW0021-LISTE            PIC  X(005).
               10  PI-DRRW0021-STEUERJAHR       PIC  9(004).
               10  PI-DRRW0021-BEZUGSJAHR       PIC  9(004).
               10  PI-DRRW0021-BUCHTEXT-FIBU    PIC  X(080).
               10  PI-DRRW0021-BELEGDATUM       PIC  X(008).
               10  PI-DRRW0021-KTY-VERARB-TYP   PIC S9(004)    BINARY.
               10  PI-DRRW0021-KNR-VERARB-TYP   PIC S9(004)    BINARY.
               10  PI-DRRW0021-ZIELSTATUS       PIC  X(001).
                   88 PI-DRRW0021-ZIELSTATUS-K       VALUE 'K'.
                   88 PI-DRRW0021-ZIELSTATUS-U       VALUE 'U'.
      *
       01  PO-DRRW0021.
         03  PO-DRRW0021-RC                     PIC  X(03).
               88  PO-DRRW0021-RC-OK                 VALUE 'OK '.
               88  PO-DRRW0021-RC-NOT-OK             VALUE 'NOK'.
      ****
      ******************** ENDE COPY DRRW0021 ************************
 
 
 
 
 
 


