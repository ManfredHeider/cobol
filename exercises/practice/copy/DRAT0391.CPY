      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0391.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRAT0391.CPv  $
      *
      *   Rev 2.1   Jan 23 2003 10:13:56   t13ae34
      *AAT0006- fachlich AAN00369 Konfiguration Daisy aus EG
      *
      *   Rev 2.0   26 Jun 2001 12:55:14   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0391  KONSTANTEN FÜR     KTY = 207       *
      *                 ZUGRIFF AUF DAISY-KONFIGURATION AUS          *
      *                 EINGEREICHTEM GESCHÄFT AKTIV/INAKTIV         *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00369!23.01.03!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  FILLER.
           05 K-KNR-ZUGRIFF-DAISY-AKTIV   PIC  X(001) VALUE 'A'.
           05 K-KNR-ZUGRIFF-DAISY-INAKTIV PIC  X(001) VALUE 'I'.
