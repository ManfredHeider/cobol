      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1435.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/cpy/DRKI1435.CPv  $
      *
      *   Rev 2.2   Aug 19 2002 15:48:26   t13ex12
      *FKI00638 Umsetzen Fonds-IDs
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR DEPOTTRANSAKTION KOMPLETTBESTAND DB24       *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEITYP  = KSDS                                            *
      *    Länge = 150 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!02.05.02!01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00638!14.08.02! 04 ! DB/24 für UPKI0043        !WHO       *
      *         !        !    !       VNR in DRKI1435     !          *
      *         !        !    !       aufgenommen         !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI1435.
           05  PI-DRKI1435-DATEN.
               10  PI-DRKI1435-PAPKNR          PIC   X(06).
               10  PI-DRKI1435-GESNR           PIC  S9(04) COMP.
               10  PI-DRKI1435-PAPKZ           PIC   X(01).
               10  PI-DRKI1435-VNR             PIC   X(15).
      *
       01  PO-DRKI1435.
           05  PO-DRKI1435-RC                  PIC  X(003).
               88 PO-DRKI1435-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1435-RC-NFD                       VALUE 'NFD'.
           05  PO-DRKI1435-ERGEBNISZEILE.
               10 PO-DRKI1435-FONDID           PIC S9(04) comp.

      ******* ENDE COPY-MEMBER **************************************
