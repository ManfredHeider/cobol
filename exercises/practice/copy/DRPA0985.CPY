      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0985.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0985.CPv  $
      *
      *   Rev 2.1   Mar 09 1999 11:27:40   T13AE41
      *Datenteil recovert
      *
      *   Rev 2.0   Feb 27 1999 14:29:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:20:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
020798*    COPY-MEMBER: DRPA0985                                      *
      *    --------------------------------------------------------   *
020798*    FUNKTIONSAUFRUF:  VERTRAG EINFüGEN - INSERT                *
      *    --------------------------------------------------------   *
020798*    ERSTELLT: GRÜNER                                           *
020798*    DATUM:    02.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
020798* AFPA0012!02.07.98! 01 ! NEU                       ! GRÜNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
020798*    FÜR DIE FUNKTION DRPA0983                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
020798 01 PI-DRPA0985.
020798    05 PI-DRPA0985-PROGRAMM              PIC  X(008).
020798    05 PI-DRPA0985-UFKT                  PIC  9(002).
020798    05 PI-DRPA0985-DATEN.
020798    COPY DRPA0988.
      *
020798 01 PO-DRPA0985.
020798    05 PO-DRPA0985-RC                         PIC  X(003).
020798       88 PO-DRPA0985-RC-OK                         VALUE 'OK '.
020798       88 PO-DRPA0985-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
