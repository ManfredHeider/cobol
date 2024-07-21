      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0894.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0894.cpv  $
      *
      *   Rev 2.1   Jul 07 1999 12:09:40   T13EX19
      *fki00077 - neue fkt für ZJKI0016
      *
      *   Rev 2.0   Apr 29 1999 13:57:16   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: drki0894                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO zjki0016                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    01.07.99                                         *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * fki00077!01007.99! 02 ! NEU                       !VZG       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION drki0894                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-drki0894.
          05 PI-drki0894-PROGRAMM           PIC  X(008).
          05 PI-drki0894-UFKT-CODE          PIC  9(002).
      *
       01 PO-drki0894.

          05 PO-drki0894-RC                 PIC  X(003).
             88 PO-drki0894-RC-OK                        VALUE 'OK '.
             88 PO-drki0894-RC-LEER                      VALUE 'LER'.
             88 PO-drki0894-RC-EOF                       VALUE 'EOF'.
             88 PO-drki0894-RC-ERR                       VALUE 'ERR'.

          05 PO-drki0894-DATEN.
             10 PO-drki0894-ADMAPERSONENID    PIC  X(13).
             10 PO-drki0894-MITARBEITERNR     PIC  S9(9) comp.
      *
      ***** Ende Copy-Member drki0894 *****************************
      *---------------------------------------------------------------*
