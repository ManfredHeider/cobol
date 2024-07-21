      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2651.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2651.CPv  $
      *
      *   Rev 2.1   Oct 24 2012 17:31:40   t13ex19
      *AKI01167: Akt. VL-Fonds überarbeiten
      *
      *   Rev 2.0   Jul 25 2012 16:28:18   t13ex19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01167!25.07.12!    ! NEU                       ! B. Stoldt*
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  PI-DRKI2651.
           05  PI-DRKI2651-DATEN.
               10  PI-DRKI2651-WPKNR           PIC   X(06).
               10  PI-DRKI2651-VLKZ            PIC   X(01).
      *
       01  PO-DRKI2651.
           05  PO-DRKI2651-RC                  PIC  X(003).
               88 PO-DRKI2651-RC-OK                 VALUE 'OK '.
               88 PO-DRKI2651-RC-NFD                VALUE 'NFD'.
           05  PO-DRKI2651-ERGEBNISZEILE.
               10 PO-DRKI2651-FONDID           PIC  S9(04) comp.
               10 PO-DRKI2651-GESNR            PIC  S9(04) COMP.


      ******* ENDE COPY-MEMBER **************************************

