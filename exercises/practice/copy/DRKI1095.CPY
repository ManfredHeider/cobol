      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1095.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki1095.cpv  $
      *
      *   Rev 2.1   13 Jul 2000 09:28:48   T13EX30
      *AKI00122
      *Änderung wg. Anpassung
      *CKV KRTARIFARTBEZ
      *
      *   Rev 2.0   Feb 09 2000 15:20:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER FÜR DIE AUFBEREITUNGSPROGRAMME CKV
      *   - MAPPING CKV TARIFART-NAMEN -ID -VOLLTARIF
      *   - EINLSEEN ÜBER VORLAUFKARTEN (80 BYTES)
      ******************************************************************
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *23.05.00!NEUERSTELLUNG                          !SPOOREN  !     *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
         01 DRKI1095-REC.
           05 DRKI1095.
      *       * CKV TARIF
              10  MAP-TARIF-NAME        PIC X(08).
      *       * TARIF-ID FÜR SUPPORT-TABELLE
              10  MAP-TARIF-ART         PIC X(03).
      *       * PRDUKT-ID-KB
      *       * = 7 FUER VOLLTARIF (V)
      *       * = 8 FUER KEIN VOLLTARIF
              10  MAP-TARIF-VOLL-KZ     PIC X(01).
           05 FILLER                    PIC X(68).

      *****************************************************************
      *        E N D E  Copy-Buch   DRKI1095  Krtarifartmap           *
      *****************************************************************
