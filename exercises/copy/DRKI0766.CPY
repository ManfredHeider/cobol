      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0766.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0766.CPv  $
      *
      *   Rev 2.2   Oct 21 2009 18:40:14   t13ex48
      *AKI00806: Neue Übertragungsart für KÜP und für Umwandlung Ko
      *KÜ --> KÜ 
      *
      *   Rev 2.1   Jan 06 2000 07:48:12   T13AE21
      *FKI00255
      *
      *   Rev 2.0   Feb 27 1999 12:58:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   06 Oct 1998 15:38:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------
      ****************************************************************
      *    COPY-MEMBER: DRKI0766  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PHILIPP                                         *
      *    DATUM:    22.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00255!22.11.99! 01 ! NEUERSTELLUNG             !PHILIPP   *
      * -------------------------------------------------------------*
201009* AKI00806!20.10.09! 01 ! Erweiterung der Funktion  !HSE       *
      *         !        !    ! DRKI0766 um VRBKOMMBETRKZ !          *
      * --------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *                                                              *
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0766                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<< ERTRAGROLLEBEZ VRBGUELTIGKEITSDATUM       *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0766.
          05 PI-DRKI0766-KEY.
             10 PI-DRKI0766-VRBPERSONENID         PIC  X(013).
             10 PI-DRKI0766-VRBPERSONENROLLEKZ    PIC  X(001).
             10 PI-DRKI0766-VRBVERTRAGSID         PIC  X(013).
          05 PI-DRKI0766-DATEN.
             10 PI-DRKI0766-VRBGUELTIGKEITSDAT    PIC  X(010).
201009       10 PI-DRKI0766-VRBKOMMBETRKZ         PIC  X(001).
      *                                                                *
       01 PO-DRKI0766.
          05 PO-DRKI0766-RC                  PIC  X(003).
             88 PO-DRKI0766-RC-OK                         VALUE 'OK '.
             88 PO-DRKI0766-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
