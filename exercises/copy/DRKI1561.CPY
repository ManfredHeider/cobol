      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1561.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1561.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 11:17:10   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 09:59:26   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * --------------------------------------------------------------*
      * AKI00565!11.07.03!    ! Neues Adressverfahren     ! ABO       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
       01 PI-DRKI1561.
           05  PI-DRKI1561-ADRESSEID    PIC X(13).

       01 PO-DRKI1561.
          05 PO-DRKI1561-RC                    PIC  X(003).
             88 PO-DRKI1561-RC-OK                         VALUE 'OK '.
             88 PO-DRKI1561-RC-NOK                        VALUE 'NOK'.

           05  PO-DRKI1561-DATEN-ANZ                PIC S9(4) COMP.
           05  PO-DRKI1561-DATEN-ALL.
             08  PO-DRKI1561-DATEN  OCCURS 300.
               10 PO-DRKI1561-PTRTABLENAME.
                  49 PO-DRKI1561-PTRTABLENAME-LEN   PIC S9(4) COMP.
                  49 PO-DRKI1561-PTRTABLENAME-TEXT  PIC X(40).
               10 PO-DRKI1561-PTRKEYDATA.
                  49 PO-DRKI1561-PTRKEYDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1561-PTRKEYDATA-TEXT    PIC X(200).
               10 PO-DRKI1561-PTRSQLDATA.
                  49 PO-DRKI1561-PTRSQLDATA-LEN     PIC S9(4) COMP.
                  49 PO-DRKI1561-PTRSQLDATA-TEXT    PIC X(3600).

      ****************************************************************
