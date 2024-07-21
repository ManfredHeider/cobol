      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2308.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2308.CPv  $
      *
      *   Rev 2.5   Aug 30 2011 12:50:16   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.4   Feb 10 2011 09:45:14   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.3   Aug 18 2010 07:28:28   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.2   Aug 13 2010 08:51:54   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.1   Aug 13 2010 08:30:08   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB Programme neu/ändern 
      *wegen neuem DTA WKUNDEN
      *
      *   Rev 2.0   Mar 18 2009 10:23:00   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
000220*----------------------------------------------------------------*
000230*                  ###############                               *
000240*         #######  D R K I 2 3 0 8  #######                      *
000250*                  ###############                               *
000260*                                                                *
000270*    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND             *
000280*          KUNDENDATENäNDERUNGEN von G66,                        *
000290*          weitere Kontoinhaber                                  *
000300*                                                                *
000310*    LAENGE =  450 BYTES      ERSTELLT  AM 26.04.02              *
000320*                                                                *
000330*                                                                *
000340*                      DATEILÄNGE = 450                          *
000350*                      DATEITYP   = QSAM                         *
000360*                                                                *
000370*                                                                *
000380*                                                                *
000390*----------------------------------------------------------------*
000400*    LOGBUCH                                                     *
000410*--------!-------------------------------------------!-----------*
000420*AUFTRAG !Datum   !Text                              !Name       *
000430*--------!-------------------------------------------!-----------*
000440*AKI00948!14.06.10! Neuerstellung                    !SWZ        *
000430*--------!-------------------------------------------!-----------*
000440*AKI01072!11.08.11! Umstellung auf Neutral           !SWZ        *
000450*----------------------------------------------------------------*
000460******************************************************************
000470*
000480 01  DRKI2308.
000490         05  BNK-SA                  PIC  X(002).
000500*            SATZART
000510         05  BNK-GES                 PIC  9(002).
000520*            GESELLSCHAFT
000530         05  BNK-VNR.
000540             10 F                    PIC  9(004).
000540             10 FILIALNUMMER         PIC  9(004).
000550             10 KUNDENNUMMER         PIC  9(007).
000560*            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
000570         05  BNK-VERARB-KZ           PIC  X(001).
000580*            VERARBEITUNGSKENNZEICHEN
000590*                I = INSERT
000600*                U = UPDATE
000610         05  BNK-PERSON-LFDNR        PIC  9(002).
000620*            '01' für erster Kontoinhaber
000630         05  BNK-NNAME               PIC  X(035).
000640*                NACHNAME DES KONTOINHABERS
000650         05  BNK-AEND-NNAME-KZ       PIC  X(001).
000660*                J = NACHNAMEÄNDERUNG
000670         05  BNK-VNAME               PIC  X(035).
000680*                VORNAME DES KONTOINHABERS
000690         05  BNK-AEND-VNAME-KZ       PIC  X(001).
000700*                J = VORNAMEÄNDERUNG
000710         05  BNK-GEBDAT              PIC  9(008).
000720*                GEBURTSTAGSDATUM    JJJJMMTT
000730         05  BNK-AEND-GEBDAT-KZ      PIC  X(001).
000740*                J = GEBURTSTAG ÄNDERUNG
000750         05  BNK-GESCHLECHT          PIC  X(001).
000760*                0   KEINE EINMELDUNG VORHANDEN
000770*                1   MAENNLICH
000780*                2   WEIBLICH
000790         05  BNK-AEND-GESCHLECHT-KZ  PIC  X(001).
000800*                J = ÄNDERUNG
000810         05  BNK-ANREDE              PIC  9(001).
000820*                ANREDEKENNZEICHEN
000830         05  BNK-AEND-ANREDE-KZ      PIC  X(001).
000840*                J = ANREDEÄNDERUNG
000850         05  BNK-STR-HAUSNR          PIC  X(028).
000860*                STRASSE + HAUSNR
000870         05  BNK-AEND-STR-HAUSNR-KZ  PIC  X(001).
000880*                J = STRASSEN/HSN-ÄNDERUNG
000970         05  BNK-PLZ                 PIC  X(009).
000980*                POSTLEITZAHL
000990         05  BNK-AEND-PLZ-KZ         PIC  X(001).
001000*                J = PLZ ÄNDERUNG
001010         05  BNK-ORT                 PIC  X(028).
001020*                ORT
001030         05  BNK-AEND-ORT-KZ         PIC  X(001).
001040*                J = ORT ÄNDERUNG
001050         05  BNK-AUSL-KZ             PIC  X(002).
001060*                AUSLANDSKENNZEICHEN
001070         05  BNK-AEND-AUSL-KZ        PIC  X(001).
001080*                J = AUSLANDSKENN. ÄNDERUNG
001210         05  BNK-NNAME-ALT           PIC  X(035).
001220*                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
001230         05  BNK-VNAME-ALT           PIC  X(035).
001240*                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND

001260         05  BNK-LETZT-AENDDAT       PIC  9(008).
001270*            Buchungsdatum aus Vorsatz
001280         05  BNK-RESERVE             PIC  X(194).




