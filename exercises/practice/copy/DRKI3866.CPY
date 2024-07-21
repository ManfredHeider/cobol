      *AUTHOR.        T13AC20.
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DBO-8361!07.04.21!NEU                              !T13AC20
      *         !        !                                 !
      *         !        !                                 !
      *         !        !                                 !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *    COPY-MEMBER: DRKI3866  FUNKTION LESE-OZP                  
      *    -------------------------------------------------------   
      *
      *   ZU VNR UND PGEB-GES WIRD VERTRAGSID UND VSNRZUSATZ GELESEN.
      *
      *****************************************************************
       01  PI-DRKI3866.
           05  PI-DRKI3866-VNR                   PIC  X(015).
           05  PI-DRKI3866-PGEB-GES              PIC S9(004) BINARY.
       01  PO-DRKI3866.
           05  PO-DRKI3866-RC                    PIC  X(003).
               88  PO-DRKI3866-RC-OK                  VALUE 'OK '.
               88  PO-DRKI3866-RC-NFD                 VALUE 'NFD'.
           05 PO-DRKI3866-VERTRAGSID             PIC  X(013).
           05 PO-DRKI3866-VSNRZUSATZ             PIC  X(010).