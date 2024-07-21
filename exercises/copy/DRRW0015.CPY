      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRRW0015.CPv  $
      *
      *   Rev 2.1   Nov 29 2013 12:31:20   t13ex71
      *AVB01291: SEPA Teil 2 - Fibu-Importdaten (BLZ und KTONR aus 
      *IBAN) für Allis Fin/Co + Diamant
      *
      *   Rev 2.0   Nov 18 2013 16:08:00   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *                                                               *
      *            COPY-MEMBER  Diamant StammdatenImport              *
      *                         Bankverbindungen Partner              *
      *            DATEINUMMER:                                       *
      *            COPYMEMBER:  DRRW0015                              *
      *            SATZLAENGE:  167 Bytes                             *
      *                                                               *
      *  Siehe:  Diamant                                              *
      *          Verfahrensdokumentation Buchungen-Partnerimport      *
      *                                                               *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01291!18.11.13! 01 ! NEU                       ! B.Pretsch*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DIAMANT-BV-SATZ.
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
           05  status-kz               PIC  X(01).
           05  tz-11                   PIC  X(01).
           05  iban                    PIC  X(34).
           05  tz-12                   PIC  X(01).
           05  bic                     PIC  X(11).

