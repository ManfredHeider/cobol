      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0840.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0840.cpv  $
      *
      *   Rev 2.1   Apr 29 1999 10:38:04   T13EX19
      *fki00029
      *
      *   Rev 2.0   Feb 27 1999 12:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:51:32   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: drki0840                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO zjki0016                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    24.04.99                                         *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * fki00029!22.04.99! 02 ! NEU                       !VZG       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION drki0840                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-drki0840.
          05 PI-drki0840-PROGRAMM           PIC  X(008).
          05 PI-drki0840-UFKT-CODE          PIC  9(002).
      *
       01 PO-drki0840.

          05 PO-drki0840-RC                 PIC  X(003).
             88 PO-drki0840-RC-OK                        VALUE 'OK '.
             88 PO-drki0840-RC-LEER                      VALUE 'LER'.
             88 PO-drki0840-RC-EOF                       VALUE 'EOF'.
             88 PO-drki0840-RC-ERR                       VALUE 'ERR'.

          05 PO-drki0840-DATEN.
             10 PO-drki0840-BENMITAROLLENID   PIC  X(13).
      *
      ***** Ende Copy-Member drki0840 *****************************
      *---------------------------------------------------------------*
