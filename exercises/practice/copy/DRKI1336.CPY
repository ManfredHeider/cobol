      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1336.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1336.CPv  $
      *
      *   Rev 2.1   Jul 30 2002 16:48:34   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.0   Dec 14 2001 09:21:02   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 1 3 3 6  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND             *
      *          KUNDENDATENäNDERUNGEN von DB24,                       *
      *          weitere Kontoinhaber                                  *
      *                                                                *
      *    LAENGE =  450 BYTES      ERSTELLT  AM 26.04.02              *
      *                                                                *
      *                                                                *
      *                      DATEILÄNGE = 450                          *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!26.04.02! Neuerstellung                    !v.d.Goltz  *
      *----------------------------------------------------------------*
      ******************************************************************
      *
       01  DRKI1336.
               05  BNK-SA                  PIC  X(001).
      *            SATZART
               05  BNK-GES                 PIC  9(002).
      *            GESELLSCHAFT
               05  BNK-VNR.
                   10 FILIALNUMMER         PIC  9(003).
                   10 KUNDENNUMMER         PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               05  BNK-VERARB-KZ           PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
               05  BNK-PERSON-LFDNR        PIC  9(002).
      *            '01' für erster Kontoinhaber
               05  BNK-NNAME               PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               05  BNK-AEND-NNAME-KZ       PIC  X(001).
      *                J = NACHNAMEÄNDERUNG
               05  BNK-VNAME               PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               05  BNK-AEND-VNAME-KZ       PIC  X(001).
      *                J = VORNAMEÄNDERUNG
               05  BNK-GEBDAT              PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               05  BNK-AEND-GEBDAT-KZ      PIC  X(001).
      *                J = GEBURTSTAG ÄNDERUNG
               05  BNK-GESCHLECHT          PIC  X(001).
      *                0   KEINE EINMELDUNG VORHANDEN
      *                1   MAENNLICH
      *                2   WEIBLICH
               05  BNK-AEND-GESCHLECHT-KZ  PIC  X(001).
      *                J = ÄNDERUNG
               05  BNK-ANREDE              PIC  9(001).
      *                ANREDEKENNZEICHEN
               05  BNK-AEND-ANREDE-KZ      PIC  X(001).
      *                J = ANREDEÄNDERUNG
               05  BNK-STR                 PIC  X(028).
      *                STRASSE/POSTFACH
               05  BNK-AEND-STR-KZ         PIC  X(001).
      *                J = STRASSENNAMEÄNDERUNG
               05  BNK-HAUSNR              PIC  X(009).
      *                Hausnr der STRASSE
               05  BNK-AEND-HAUSNR-KZ      PIC  X(001).
      *                J = ÄNDERUNG Hausnr.
               05  BNK-POSTFACH            PIC  X(009).
      *                POSTFACH
               05  BNK-AEND-PF-KZ          PIC  X(001).
      *                J = ÄNDERUNG Postfach
               05  BNK-PLZ                 PIC  X(009).
      *                POSTLEITZAHL
               05  BNK-AEND-PLZ-KZ         PIC  X(001).
      *                J = PLZ ÄNDERUNG
               05  BNK-ORT                 PIC  X(028).
      *                ORT
               05  BNK-AEND-ORT-KZ         PIC  X(001).
      *                J = ORT ÄNDERUNG
               05  BNK-AUSL-KZ             PIC  X(003).
      *                AUSLANDSKENNZEICHEN
               05  BNK-AEND-AUSL-KZ        PIC  X(001).
      *                J = AUSLANDSKENN. ÄNDERUNG
               05  BNK-VORWAHL-TEL         PIC  9(006).
      *                Vorwahl zur TELEFON-NR.
               05  BNK-AEND-VORWAHL-KZ     PIC  X(001).
      *                J = Vorwahl Telefonnr. ÄNDERUNG
               05  BNK-TEL                 PIC  9(009).
      *                TELEFON-NR.
               05  BNK-AEND-TEL-KZ         PIC  X(001).
      *                J = TELEFONNR ÄNDERUNG
               05  BNK-VOR-TEL-ALT         PIC  9(006).
      *                VORHERIGE Vorwahl TELEFONNR DES KONTOINHABERS
               05  BNK-TEL-ALT             PIC  9(009).
      *                VORHERIGE TELEFONNR DES KONTOINHABERS
               05  BNK-NNAME-ALT           PIC  X(035).
      *                  NACHNAME DES KONTOINHABERS AUS KOMPLETTBESTAND
               05  BNK-VNAME-ALT           PIC  X(035).
      *                  VORNAME DES KONTOINHABERS AUS KOMPLETTBESTAND

               05  BNK-LETZT-AENDDAT       PIC  9(008).
      *            Buchungsdatum aus Vorsatz
               05  BNK-RESERVE             PIC  X(147).



