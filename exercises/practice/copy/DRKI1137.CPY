      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1137.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI1137.CPv  $
      *
      *   Rev 2.1   31 Jan 2001 14:25:40   T13EX39
      *AKI00068 KBI
      *
      *   Rev 1.0   07 Jul 1998 11:49:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1137                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1137                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  30.08.00/18.07.34
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00068!30.10.00!1.0 ! ZUS. SELECT               ! WHO      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1137
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01 PI-DRKI1137.
          05 PI-DRKI1137-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1137-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1137-KEY.
            10 PI-DRKI1137-VETVERTRAGSID          PIC X(013).

      *
       01  PO-DRKI1137.
           05  PO-DRKI1137-RC                 PIC  X(003).
               88 PO-DRKI1137-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1137-RC-EOF                       VALUE 'EOF'.
               88 PO-DRKI1137-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1137-DATEN.
               10 PO-DRKI1137-VETVERSTARLFDNR     PIC S9(4) COMP.
               10 PO-DRKI1137-VETPRODUKTID        PIC S9(4) COMP.
               10 PO-DRKI1137-VETVERSSUMME        PIC S9(9)V9(2) COMP-3.
               10 PO-DRKI1137-VETVERSSUMMEWKZ     PIC X(01).
               10 PO-DRKI1137-VETSUBTYPE          PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************
