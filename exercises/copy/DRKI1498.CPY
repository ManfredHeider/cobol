      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1498.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1498.CPv  $
      *
      *   Rev 2.1   Jan 27 2003 11:01:18   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.0   Oct 16 2002 08:33:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1498                                     *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI999                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  19.11.2002/18:08:24
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00162!14.11.02!01.0! Neu                       !WHO       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1498
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI1498.
          05 PI-DRKI1498-PROGRAMM               PIC X(008).
          05 PI-DRKI1498-UFKT                   PIC 9(002).
          05 PI-DRKI1498-DATEN.
             10 PI-DRKI1498-FOZFONDID            PIC S9(4) COMP.
             10 PI-DRKI1498-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1498-FOZLFDNR             PIC S9(4) COMP.
             10 PI-DRKI1498-FOZVERTRAGSID        PIC X(13).
             10 PI-DRKI1498-FOZEINAUSTYP         PIC X(01).
      *
       01 PO-DRKI1498.

          05 PO-DRKI1498-RC                     PIC  X(003).
             88 PO-DRKI1498-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1498-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1498-DATEN.
             10 PO-DRKI1498-FOZFONDID            PIC S9(4) COMP.
             10 PO-DRKI1498-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PO-DRKI1498-FOZLFDNR             PIC S9(4) COMP.
             10 PO-DRKI1498-FOZVERTRAGSID        PIC X(13).
      *
      ******* ENDE COPY-MEMBER **************************************
      *

