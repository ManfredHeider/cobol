      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0141.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0141.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 Oct 1997 10:40:34   T13AE35
      *AFVB0376 - Neuerstellung Uebergabebereich PMPA0016-UPVB0100
      *
      *   Rev 1.0   Feb 19 1997 09:57:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0141                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0141                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Hering                                          *
      *    DATUM:    05.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0364!05.08.97! 01 ! NEU                       ! HERING   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0141
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *
      ****************************************************************
      *
       01  PI-DRVB0141.
           05  PI-DRVB0141-TITEL-AKAD         PIC  X(010).
           05  PI-DRVB0141-TITEL-ADEL         PIC  X(010).
           05  PI-DRVB0141-NAME               PIC  X(040).
           05  PI-DRVB0141-ANREDE             PIC  X(080).
      *
       01  PO-DRVB0141.
           05  PO-DRVB0141-RC                 PIC  X(003).
               88 PO-DRVB0141-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0141-RC-NOK                       VALUE 'NOK'.
           05  PO-DRVB0141-ANREDE-TEXT        PIC  X(080).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
