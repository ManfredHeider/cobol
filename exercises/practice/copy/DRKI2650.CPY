      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2650.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2650.CPv  $
      *
      *   Rev 2.1   Oct 24 2012 17:31:34   t13ex19
      *AKI01167: Akt. VL-Fonds überarbeiten
      *
      *   Rev 2.0   Jul 25 2012 16:28:12   t13ex19
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

       01  PI-DRKI2650.
           05  PI-DRKI2650-DATEN.
               10  PI-DRKI2650-ISIN            PIC   X(12).
               10  PI-DRKI2650-VLKZ            PIC   X(01).
      *
       01  PO-DRKI2650.
           05  PO-DRKI2650-RC                  PIC  X(003).
               88 PO-DRKI2650-RC-OK                 VALUE 'OK '.
               88 PO-DRKI2650-RC-NFD                VALUE 'NFD'.
           05  PO-DRKI2650-ERGEBNISZEILE.
               10 PO-DRKI2650-FONDID           PIC  S9(04) comp.
               10 PO-DRKI2650-GESNR            PIC  S9(04) COMP.


      ******* ENDE COPY-MEMBER **************************************

