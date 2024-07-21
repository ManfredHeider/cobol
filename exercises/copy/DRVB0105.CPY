      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0105.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0105.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Dec 17 1996 14:23:56   P13SP17
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0105, SCHNITTSTELLE UPVB0090             *
      *                 ERMITTELT STELLENTYP ZU EINER STELLEN-NUMMER *
      *                 UND WENN VORHANDEN DEREN SUB-TYP             *
      *    --------------------------------------------------------  *
      *    ERSTELLT: SCHMIDT                                         *
      *    DATUM:    09.08.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0299!09.08.96!    ! NEU                       ! SCHMIDT  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *   PI-UPVB0090-ST-NR
      *         STELLENNUMMER, FÜR DAS STELLENTYP UND SUB-TYP
      *         ERMITTELT WERDEN SOLLEN
      *
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-UPVB0090.
           05  PI-UPVB0090-ST-NR              PIC  9(007).
      *
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-UPVB0090.

      *    * ERGEBNIS:
           05  PO-UPVB0090-RC                 PIC  9(001).
               88  PO-VB90-RC-OK                           VALUE 0.
               88  PO-VB90-RC-NOT-FOUND                    VALUE 8.

      *    * KTY-NR FÜR STELLENTYP
           05  PO-UPVB0090-KTY-ST-TYP        PIC S9(004) BINARY.

      *    * KNR DES ERMITTELTEN STELLEN-TYPS
           05  PO-UPVB0090-KNR-ST-TYP        PIC S9(004) BINARY.

      *    * KURZBEZEICHNUNG DES ERMITTELTEN STELLEN-TYPS
           05  PO-UPVB0090-ST-BEZ-KURZ       PIC X(004).

      *    * KTY-NR FÜR SUB-TYP
           05  PO-UPVB0090-KTY-SUB-TYP       PIC S9(004) BINARY.

      *    * KNR DES ERMITTELTEN SUB-TYPS
           05  PO-UPVB0090-KNR-SUB-TYP       PIC S9(004) BINARY.

      *    * KURZBEZEICHNUNG DES ERMITTELTEN SUB-TYPS
           05  PO-UPVB0090-SUB-BEZ-KURZ       PIC X(004).


      ******** ENDE COPY-MEMBER   DRVB0105   ***********************

