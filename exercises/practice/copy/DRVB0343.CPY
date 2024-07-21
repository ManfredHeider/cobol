      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0343.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0343.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Dec 1998 11:37:22   T13AE28
      *AOP00161 - Neuerstellung
      *
      *   Rev 1.0   27 Oct 1998 16:56:28   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0343                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0343                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                       *
      *    DATUM:    25.11.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !25.11.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0343
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *    FUNKTION:
      *            ERMITTELN DES DATUMS AUS DEM KOMPL.KEY
      *            IM FORMAT JJMMTT
      ****************************************************************
      *
       01  PI-DRVB0343.
           05  PI-DRVB0343-KOMPL-KEY          PIC  9(006).
      *
       01  PO-DRVB0343.
           05  PO-DRVB0343-RC                 PIC  X(003).
               88 PO-DRVB0343-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0343-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0343-RC-DAT-NOK                   VALUE 'DAT'.
           05  PO-DRVB0343-DATUM-1            PIC  9(006).
      *
      *
      ******* ENDE COPY-MEMBER **************************************
