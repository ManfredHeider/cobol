      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1914.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1914.CPv  $
      *
      *   Rev 2.1   Oct 30 2006 10:21:08   t13ex48
      *AKI00551: Freistellungsaufträge
      *
      *   Rev 2.0   Apr 19 2006 11:18:14   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1914                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1914                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.03.2006/16:49:26
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1914
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1914.
           05  PI-DRKI1914-KEY.


             10 PI-DRKI1914-FSAPERSONENID   PIC X(13).
             10 PI-DRKI1914-FSAVERTRAGSID   PIC X(13).


           05  PI-DRKI1914-DATEN.


             10 PI-DRKI1914-FSAANZDEPFREISTBET   PIC S9(4) COMP.
             10 PI-DRKI1914-FSAFREIBETRAGABL   PIC X(10).
             10 PI-DRKI1914-FSAFREIBETRAGVERBR   PIC S9(9)V9(2)  COMP-3.
             10 PI-DRKI1914-FSAGEMFREIBETRAG   PIC S9(9)V9(2)  COMP-3.
             10 PI-DRKI1914-FSAGUELTIGAB   PIC X(10).
             10 PI-DRKI1914-FSAPARTNERGESID   PIC S9(4)  COMP.
             10 PI-DRKI1914-FSASTUECKZINS   PIC S9(9)V9(2)  COMP-3.
             10 PI-DRKI1914-FSAWAEHRUNGSKZ   PIC X(1).
             10 PI-DRKI1914-FSAZINSABSCHSTEUER   PIC S9(9)V9(2)  COMP-3.

      *
       01  PO-DRKI1914.
           05  PO-DRKI1914-RC                 PIC  X(003).
               88 PO-DRKI1914-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1914-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
