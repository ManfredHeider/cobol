      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1411.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1411.CPv  $
      *
      *   Rev 2.1   Jun 13 2002 07:43:56   t13ex49
      *AKI00274 - RDTADB24 
      *Select für ZXKI0359
      *
      *   Rev 2.0   Apr 23 2002 16:01:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1411                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                       *
      *    DATUM:    23.04.2002                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!23.04.02! 01 ! NEUERSTELLUNG             !Peter Pohl*
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1411                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1411.
          05 PI-DRKI1411-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1411-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1411-DATEN.
            07 PI-DRKI1411-VRBPERSONENID          PIC  X(013).
            07 PI-DRKI1411-VRBPERSONENROLLEKZ     PIC  X(001).
            07 PI-DRKI1411-VRBVERTRAGSIDVON       PIC  X(013).
            07 PI-DRKI1411-VRBVERTRAGSIDBIS       PIC  X(013).
      *
       01 PO-DRKI1411.

          05 PO-DRKI1411-RC                       PIC  X(003).
             88 PO-DRKI1411-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1411-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI1411-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1411-DATEN.
            07 PO-DRKI1411-VRBVERTRAGSID          PIC X(013).
      *
      ***** Ende Copy-Member DRKI1411 *****************************

