      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.08.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1824.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1824.CPv  $
      *
      *   Rev 2.1   Aug 26 2005 10:09:54   t13ex39
      *FKI00965: Aktualisieren FONDS
      *
      *   Rev 2.0   Aug 15 2005 11:09:26   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1824                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1219                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00965!13.08.05! 1.0! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1824
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
      *
       01  PI-DRKI1824.
           05  PI-DRKI1824-KEY.

             10 PI-DRKI1824-FONDISIN      PIC X(12).
             10 PI-DRKI1824-FONDWERTPAPKZ PIC X(01).

      *
       01  PO-DRKI1824.
           05  PO-DRKI1824-RC                 PIC  X(003).
               88 PO-DRKI1824-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1824-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1824-DATEN.

             10 PO-DRKI1824-FONDID    PIC S9(4) COMP.

             10 PO-DRKI1824-FONDART   PIC X(20).
             10 PO-DRKI1824-FONDBEZEICHNUNG   PIC X(40).
             10 PO-DRKI1824-FONDDVAGPRODSCHL   PIC S9(4) COMP.
             10 PO-DRKI1824-FONDGESNR   PIC S9(4) COMP.
             10 PO-DRKI1824-FONDISIN   PIC X(12).
             10 PO-DRKI1824-FONDWERTPAPKNR   PIC X(6).
             10 PO-DRKI1824-FONDWERTPAPKZ   PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************

