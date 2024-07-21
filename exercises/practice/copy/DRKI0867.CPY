      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0867.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0867.CPv  $
      *
      *   Rev 2.2   Oct 01 1999 09:05:26   T13AE21
      *AKI00029
      *
      *   Rev 2.1   Aug 31 1999 11:24:16   T13AE21
      *AKI00029
      *
      *   Rev 2.0   Apr 29 1999 13:57:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0867                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                          *
      *    DATUM:    02.06.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00029!02.06.99! 01 ! NEUERSTELLUNG             ! PHILIPP  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0867                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *           DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0867.
          05 PI-DRKI0867-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0867-UFKT-CODE                PIC  9(002).
          05 PI-DRKI0867-DATEN.
            10 PI-DRKI0867-VETVERTRAGSID          PIC X(013).
      *     10 PI-DRKI0867-VETVERSTARLFDNR        PIC S9(04)COMP.
      *
       01 PO-DRKI0867.

          05 PO-DRKI0867-RC                       PIC  X(003).
             88 PO-DRKI0867-RC-OK                    VALUE 'OK '.
             88 PO-DRKI0867-RC-NOT-FOUND             VALUE 'NFD'.
             88 PO-DRKI0867-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI0867-DATEN.
            10 PO-DRKI0867-VETPRODUKTID           PIC S9(04) COMP.
            10 PO-DRKI0867-VETVERSTARLFDNR        PIC S9(04) COMP.
            10 PO-DRKI0867-VETSUBTYPE             PIC X(01).
      *
      ***** Ende Copy-Member DRKI0867 *****************************
      *---------------------------------------------------------------*
