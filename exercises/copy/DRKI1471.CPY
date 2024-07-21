      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1471.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1471.CPv  $
      *
      *   Rev 2.1   Nov 28 2002 08:44:42   t13ex49
      *AKI00294 - ISIN
      *
      *   Rev 2.0   Oct 16 2002 08:30:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00294!18.19.02!    ! NEU                       !Peter Pohl*
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRKI1471.
           05  PI-DRKI1471-DATEN.
               10  PI-DRKI1471-ISIN            PIC   X(12).
               10  PI-DRKI1471-GESNR           PIC  S9(04) COMP.
               10  PI-DRKI1471-PAPKZ           PIC   X(01).
               10  PI-DRKI1471-VNR             PIC   X(15).
      *
       01  PO-DRKI1471.
           05  PO-DRKI1471-RC                  PIC  X(003).
               88 PO-DRKI1471-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1471-RC-NFD                       VALUE 'NFD'.
           05  PO-DRKI1471-ERGEBNISZEILE.
               10 PO-DRKI1471-FONDID           PIC S9(04) comp.

      ******* ENDE COPY-MEMBER **************************************



