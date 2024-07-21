      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0149.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0149.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 Aug 1997 17:44:18   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.0   Feb 19 1997 12:22:40   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: drat0149  Funktion uebergabe VSTATUS v von   *
      *                 UPAT0018
      *    -------------------------------------------------------   *
      *    ERSTELLT: Opritz                                          *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0022!14.08.97! 01 ! NEUERSTELLUNG             ! Opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0149: AUFRUF VON UPAT0018
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    ALLES UEBER UPAT0018
      ****************************************************************
      *
       01  PI-DRAT0149.
           05  PI-DRAT0149-V-DATEN.
               10 PI-DRAT0149-PVST-VSTAT          PIC  X(004).
      *
       01  PO-DRAT0149.
           05  PO-DRAT0149-RC                     PIC X(003).
               88 PO-DRAT0149-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0149-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRAT0149-DATEN.
               10 PO-DRAT0149-PVST-MELDUNG.
                  15 PO-DRAT0149-PVST-attribut    PIC  X(001).
                  15 PO-DRAT0149-PVST-TEXT        PIC  X(079).

      ******** ENDE COPY-MEMBER Übergabebereich        (DRAT0149) *****
