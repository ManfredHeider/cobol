      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0017.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRRW0017.CPv  $
      *
      *   Rev 2.7   Oct 01 2019 17:51:34   t13ex71
      *FRW00090: Ausbilder-Bonus Diskontgutschriften 09/2019 - Copy
      *books und Upros sind auf M:ED nicht aktuell; neu aus- und wi
      *ecken (BPR)
      *
      *   Rev 2.6   Jun 27 2019 17:24:10   t13ex71
      *ASL00589: Verrechnung freiwillige Sonderprovisionen mit Soll
      *salden - UPRW0003/4/5 mit Copybooks (BPR)
      *
      *   Rev 2.5   Apr 16 2019 15:22:20   t13ex71
      *ASL00589: Verrechnung freiwillige Sonderprovisionen mit Soll
      *salden - UPRW0003/4/5 mit Copybooks (BPR)
      *
      *   Rev 2.4   Jul 27 2016 12:23:34   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Frei UPRW00
      *05 (HWO)
      *
      *   Rev 2.3   Jul 22 2016 11:11:58   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Frei UPRW00
      *05 (HWO)
      *
      *   Rev 2.2   Jun 14 2016 14:45:42   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Frei UPRW0005
      *
      *   Rev 2.1   May 31 2016 14:36:44   t13ex30
      *AVB01400: Projekt Lucy2 - FIBU-Schnittstelle UST-Frei UPRW0005
      *
      *   Rev 2.0   May 17 2016 09:56:02   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRRW0017                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    17.05.2016                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!17.05.16!2.1 ! NEU                       ! H. WÖLFLIK*
180119* ASL00589!18.01.19!2.2 ! Sollsaldo DVAG/ALLF1/ALLF2! B.Petsch  *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRRW0016 - Buchen FIBU-Schnittstelle UPRW0005     *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRRW0017.
      ***  Daten
           05  PI-DRRW0017-DATEN.
               10  PI-DRRW0017-MA-ST-NR         PIC S9(009)    BINARY.
               10  PI-DRRW0017-PZ               PIC  X(001).
               10  PI-DRRW0017-DIR              PIC  9(003).
               10  PI-DRRW0017-STUFE            PIC  X(005).
               10  PI-DRRW0017-NAME             PIC  X(040).
               10  PI-DRRW0017-VORNAME          PIC  X(020).
               10  PI-DRRW0017-BETR-FREI        PIC S9(09)V99.
180119         10  PI-DRRW0017-BETR-DISKKTO     PIC S9(09)V99.
180119         10  PI-DRRW0017-SOLLSA-BES433    PIC  X(001).
180119             88 PI-DRRW0017-SOLLSA-BES433-J    VALUE 'J'.
180119             88 PI-DRRW0017-SOLLSA-BES433-N    VALUE 'N'.
               10  PI-DRRW0017-HINWEIS          PIC  X(025).
               10  PI-DRRW0017-TURNUS-JJJJMM    PIC  9(006).
               10  FILLER REDEFINES PI-DRRW0017-TURNUS-JJJJMM.
                   15 PI-DRRW0017-TURNUS-JJJJ.
                      20 PI-DRRW0017-TURNUS-JH  PIC  9(002).
                      20 PI-DRRW0017-TURNUS-JJ  PIC  9(002).
                   15 PI-DRRW0017-TURNUS-MM     PIC  9(002).
               10  PI-DRRW0017-GESELL           PIC  9(001).
               10  PI-DRRW0017-GESELLSCHAFT     PIC  X(012).
               10  PI-DRRW0017-ANSPR-GRUND      PIC S9(004)    BINARY.
               10  PI-DRRW0017-ANSPR-STATUS     PIC  X(001).
                   88 PI-DRRW0017-ANSPR-AUSZAHLEN    VALUE 'A'.
                   88 PI-DRRW0017-ANSPR-NICHT-AUSZ   VALUE 'N'.
               10  PI-DRRW0017-BUCHTEXT         PIC  X(035).
               10  PI-DRRW0017-MANDANT          PIC  X(003).
               10  PI-DRRW0017-BELEGART         PIC  X(003).
               10  PI-DRRW0017-KOSTENSTELLE     PIC  X(004).
               10  PI-DRRW0017-KONTO-SOLL       PIC  X(012).
               10  PI-DRRW0017-KONTO-HABEN      PIC  X(012).
               10  PI-DRRW0017-LISTE            PIC  X(005).
               10  PI-DRRW0017-JOB              PIC  X(006).
               10  PI-DRRW0017-STEUERJAHR       PIC  9(004).
               10  PI-DRRW0017-BEZUGSJAHR       PIC  9(004).
               10  PI-DRRW0017-BUCHTEXT-FIBU    PIC  X(080).
               10  PI-DRRW0017-BELEGDATUM       PIC  X(008).
      *
       01  PO-DRRW0017.
         03  PO-DRRW0017-RC                     PIC  X(03).
               88  PO-DRRW0017-RC-OK                 VALUE 'OK '.
               88  PO-DRRW0017-RC-NOT-OK             VALUE 'NOK'.
      ****
      ******************** ENDE COPY DRRW0017 ************************
 
 
 
 
 


