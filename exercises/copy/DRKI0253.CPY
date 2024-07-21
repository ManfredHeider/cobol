      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0253.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0253.CPv  $
      *
      *   Rev 2.1   Apr 23 1999 14:41:58   T13EX19
      * 
      *
      *   Rev 2.0   Feb 27 1999 12:54:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:48:58   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   07 Jul 1998 11:54:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0253 Funktion INSERT NATUERLICHE PERSON  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Veit                                            *
      *    DATUM:    08.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!08.07.98! 01 ! NEUERSTELLUNG             ! Veit     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0253                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<        PERSONENROLLE 'PERO'               *
      ****************************************************************
      *
       01 PI-DRKI0253.
          05 PI-DRKI0253-PERSKEY.
             10 PI-DRKI0253-PERSROLLENID       PIC X(13).
             10 PI-DRKI0253-PERSROLLENKZ       PIC X(1).
          05 PI-DRKI0253-LAST-UPDATE           PIC  X(026).
      *
       01 PO-DRKI0253.
          05 PO-DRKI0253-RC                    PIC  X(003).
             88 PO-DRKI0253-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0253-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
      *---------------------------------------------------------------*
