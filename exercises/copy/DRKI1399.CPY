      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1399.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1399.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 11:15:10   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   Apr 09 2002 08:33:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1399                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZTXKI0002                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Peter Pohl                                       *
      *    DATUM:    21.03.2002                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00114!21.03.02! 01 ! NEUERSTELLUNG             !Peter Pohl*
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1399                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1399.
          05 PI-DRKI1399-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1399-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1399-DATEN.
            07 PI-DRKI1399-AUSL-KZ                PIC  X(003).
            07 PI-DRKI1399-PLZ                    PIC  X(009).
            07 PI-DRKI1399-ADRESSESUCHSTRASSE     PIC  X(045).
            07 PI-DRKI1399-ADRESSEGEPRUEFT        PIC  X(001).
      *
       01 PO-DRKI1399.

          05 PO-DRKI1399-RC                       PIC  X(003).
             88 PO-DRKI1399-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1399-RC-NOT-FOUND             VALUE 'NFD'.

          05 PO-DRKI1399-DATEN.
            07 PO-DRKI1399-ADRESSEID              PIC  X(013).
      *
      ***** Ende Copy-Member DRKI1399 *****************************

