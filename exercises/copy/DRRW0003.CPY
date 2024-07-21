      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0003.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRRW0003.CPv  $
      *
      *   Rev 2.5   Nov 29 2013 12:31:10   t13ex71
      *AVB01291: SEPA Teil 2 - Fibu-Importdaten (BLZ und KTONR aus 
      *IBAN) für Allis Fin/Co + Diamant
      *
      *   Rev 2.4   Oct 11 2013 13:38:46   t13ex71
      *AVB01291: SEPA Teil 2 - PMRW0035 (BLZ und KTONR aus IBAN neh
      *men)  
      *
      *   Rev 2.3   Oct 04 2013 14:13:10   t13ex71
      *AVB01291: SEPA Teil 2 - PMRW0035 (BLZ und KTONR für AT aus I
      *BAN nehmen)  
      *
      *   Rev 2.2   Oct 04 2013 11:22:12   t13ex71
      *AVB01291: SEPA Teil 2 - PMRW0035 (BLZ und KTONR für AT aus I
      *BAN nehmen)  
      *
      *   Rev 2.1   Apr 03 2013 13:28:30   i13az40
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *DRRW0003  u
      *nd PMRW0035 
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:26:28   T13AE35
      *AIS00009 - Neuerstellung, SST Bankverb.-Import fuer FIBU
      *
      *   Rev 1.0   10 Mar 1998 17:18:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *                                                               *
      *            COPY-MEMBER  AllData StammdatenImport              *
      *                         Bankverbindungen Partner              *
      *            DATEINUMMER:                                       *
      *            COPYMEMBER:  DRRW0003                              *
      *            SATZLAENGE:  120 Bytes                             *
      *                                                               *
      *  Siehe:  ALLIS FINCO                                          *
      *          Verfahrensdokumentation Buchungen-Partnerimport      *
      *                                                               *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00030!19.03.98!    ! NEU                       ! Moll     *
      * AVB01244!03.04.13!    ! IBAN und BIC              !Haralanova*
011013* AVB01291!01.10.13!    ! IBAN und BIC entfernen    !B.Pretsch *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  AFC-BV-SATZ.
      *    * Feld 1: VB-Nummer (7st. + Prüfziffer) in Feld partner
      *    05  partner                 PIC  X(12).
           05  VBNr                    PIC  X(08).
           05  FILLER                  PIC  X(04).

           05  tz-01                   PIC  X(01).
           05  bvNr                    PIC  X(06).
           05  tz-02                   PIC  X(01).
           05  gueltigAb               PIC  X(08).
           05  tz-03                   PIC  X(01).
           05  gueltigBis              PIC  X(08).
           05  tz-04                   PIC  X(01).
           05  hauptBv                 PIC  X(01).
           05  tz-05                   PIC  X(01).
           05  bank                    PIC  X(08).
           05  tz-06                   PIC  X(01).
           05  kontoNr                 PIC  X(20).
           05  tz-07                   PIC  X(01).
           05  kontoInhaber            PIC  X(30).
           05  tz-08                   PIC  X(01).
           05  anlageDatum             PIC  X(08).
           05  tz-09                   PIC  X(01).
           05  aenderungsDatum         PIC  X(08).
           05  tz-10                   PIC  X(01).
      *    *   Neu:1 Änderung:2 Löschung:9
           05  status-kz               PIC  X(01).
011013*--- IBAN und BIC vom 03.04.2013 wieder entfernen
011013*    05  tz-11                   PIC  X(01).
011013*    05  iban                    PIC  X(34).
011013*    05  tz-12                   PIC  X(01).
011013*    05  bic                     PIC  X(11).

