      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1094.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drki1094.cpv  $
      *
      *   Rev 2.2   08 Nov 2000 15:14:54   T13EX58
      * 
      *
      *   Rev 2.1   13 Jul 2000 09:28:42   T13EX30
      *AKI00122
      *Änderung wg. Anpassung
      *CKV KRTARIFARTBEZ
      *
      *   Rev 2.0   Feb 09 2000 15:20:28   P13SP20
      *Archiverstellung
      *
      *-----------------------------------------------------------------*
      *******************************************************************
      *    SUPPORT                                                      *
      *    COPY-MEMBER FÜR DIE CKV-ERMITTLUNGSGS und AUFBEREITUNGS-     *
      *    PROGRAMME                                                    *
      *                                                                 *
      *    IN DIE TABELLE WERDEN DIE MAPPINGDATEN FÜR:                  *
      *    - DIE ZUORDNUNG TARIFNAMEN ZU TARIFART FUER KVTARIFART       *
      *    - DIE PRÜFUNG AUF VOLLTARIFE                                 *
      *    VORGENOMMEN
      *                                                                 *
      *    DURCH DIESES COPY WERDEN DSKI0326 u. DRKI0881 ERSETZT        *
      *******************************************************************
      * AENDERUNGEN:                                                    *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME        *
      *---------------------------------------------------------------- *
      * AKI00022!23.05.00!1.0 ! NEUERSTELLUNG             !Spooren      *
      *         !        !    !                           !             *
      *         !        !    !                           !             *
      *         !        !    !                           !             *
      *         !        !    !                           !             *
      * ENDE-LOGBUCH                                                    *
      *******************************************************************
        01  K-TARIF-ART-TAB.
            05  K-TARIF-ART-OCC                   OCCURS  1998.
                10  K-TARIF-NAME        PIC X(08).
                10  K-TARIF-ART         PIC 9(03).
      *         * 7 = VOLLTARIF; 8 = KEIN VOLLTARIF
                10  K-TARIF-VOLL-KZ     PIC 9(01).
        01  K-TARIF-ART-MAX             PIC S9(4) COMP VALUE 1998.
        01  K-TARIF-ART-DEFAULT         PIC S9(4) COMP VALUE -2.
        01  S-TARNAM                    PIC X(08)  VALUE SPACES.

      *****************************************************************
      *        E N D E  Copy-Buch   DRKI1094  Krtarifartmap           *
      *****************************************************************

